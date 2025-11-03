namespace WoofWare.PrattParser

/// Specification of how to parse things which act like brackets: that is, they start with a token,
/// then consume some stuff, then there's another token to mark the end.
///
/// Optionally you can specify that the bracket-like token consumes something at the end too:
/// for example, `if...then...` does not have a trailing "end-if".
/// The trailing clause will consume as much as it can, so e.g. `if foo then bar!` would parse as
/// `if foo then (bar!)`.
///
/// Optionally you can specify that the bracket-like token consumes something at the beginning too:
/// for example, `a.[3]` is a bracket pair `.[` and `]` with two inputs.
/// Note that you could use this to implement binary operators, but they will bind as loosely as possible
/// if you do this, and it's less efficient, and it's probably confusing to think about associativity.
///
/// Optionally you can specify a single construct with multiple delimiters:
/// for example, `if...then...else...` consumes three expressions.
type BracketLikeParser<'tokenTag, 'expr> =
    {
        /// Whether to consume input after the final token, e.g. like `if...then...else...` consumes,
        /// whereas `(...)` does not.
        ConsumeAfterFinalToken : bool
        /// Whether to consume the input before the initial token, e.g. like `a.[5]` consumes the `a`.
        ConsumeBeforeInitialToken : bool
        /// The successive list of delimiters after the initial delimiter that "opens the brackets".
        /// For example, this might be `[then]`, or `[then ; else]`, or `[')']`.
        BoundaryTokens : 'tokenTag list
        /// How to build an expression given that you've got all the constituent chunks that came
        /// between the delimiters.
        ///
        /// We guarantee that the input list will have (as many elements as BoundaryTokens)+1
        /// if ConsumeAfterFinalToken is true, or as many elements as BoundaryTokens
        /// if ConsumeAfterFinalToken is false.
        ///
        /// Each element is Some expr if that segment contained content, or None if the segment
        /// was empty (i.e., the next token was immediately a boundary token).
        /// For example, `[]` would pass `[None]` to a construct with one boundary token.
        Construct : 'expr option list -> 'expr
    }

/// An entity which knows how to parse a stream of 'tokens into an 'expr.
type Parser<'tokenTag, 'token, 'expr> when 'tokenTag : comparison =
    private
        {
            GetTag : 'token -> 'tokenTag
            UnaryPrefix : Map<'tokenTag, (unit * int) * ('expr -> 'expr)>
            UnaryPostfix : Map<'tokenTag, (int * unit) * ('expr -> 'expr)>
            Infix : Map<'tokenTag, (int * int) * ('expr -> 'expr -> 'expr)>
            Atom : string -> 'token -> 'expr option
            BracketLike : Map<'tokenTag, BracketLikeParser<'tokenTag, 'expr> list>
        }

/// Module for constructing and executing Parsers.
[<RequireQualifiedAccess>]
module Parser =
    /// The basic parser with the minimum possible information.
    /// You specify how to take a token and get a tag from it,
    /// and you specify how to convert atoms (such as constant ints, or variables) into expressions.
    ///
    /// The atom-parsing function is given the entire source string, as well as the 'token
    /// of which we are asking "is this an atom, and if so, how shall it be represented in the AST?".
    let make<'tokenTag, 'token, 'expr when 'tokenTag : comparison>
        (getTag : 'token -> 'tokenTag)
        (atoms : string -> 'token -> 'expr option)
        : Parser<'tokenTag, 'token, 'expr>
        =
        {
            GetTag = getTag
            UnaryPrefix = Map.empty
            UnaryPostfix = Map.empty
            Infix = Map.empty
            Atom = atoms
            BracketLike = Map.empty
        }

    /// Add a prefix operator to this parser.
    /// The precedence is an int, where higher numbers bind more tightly.
    /// (Following [matklad](https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html), we
    /// express this as `unit * int` to make it clear that it's binding on the right.)
    let withUnaryPrefix<'tokenTag, 'token, 'expr when 'tokenTag : comparison>
        (tokenType : 'tokenTag)
        (precedence : unit * int)
        (construct : 'expr -> 'expr)
        (parser : Parser<'tokenTag, 'token, 'expr>)
        : Parser<'tokenTag, 'token, 'expr>
        =
        { parser with
            UnaryPrefix =
                parser.UnaryPrefix
                |> Map.change
                    tokenType
                    (fun existing ->
                        match existing with
                        | None -> Some (precedence, construct)
                        | Some _ -> failwithf "Duplicate unary prefix parser added for token %+A" tokenType
                    )
        }

    /// Add a postfix operator to this parser.
    /// The precedence is an int, where higher numbers bind more tightly.
    /// (Following [matklad](https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html), we
    /// express this as `int * unit` to make it clear that it's binding on the left.)
    let withUnaryPostfix<'tokenTag, 'token, 'expr when 'tokenTag : comparison>
        (tokenType : 'tokenTag)
        (precedence : int * unit)
        (construct : 'expr -> 'expr)
        (parser : Parser<'tokenTag, 'token, 'expr>)
        : Parser<'tokenTag, 'token, 'expr>
        =
        { parser with
            UnaryPostfix =
                parser.UnaryPostfix
                |> Map.change
                    tokenType
                    (fun existing ->
                        match existing with
                        | None -> Some (precedence, construct)
                        | Some _ -> failwithf "Duplicate unary postfix parser added for token %+A" tokenType
                    )
        }

    /// Add a binary infix operator to this parser.
    /// The precedence is a pair of ints, where higher numbers bind more tightly.
    ///
    /// For example, to make an operator associate on the left, you would give it
    /// tighter (higher-precedence) binding on the right, whereupon parsing would proceed as follows:
    ///
    /// 1 + 2 + 3 := 1 +2 +3
    ///
    /// after which the only possible bracketing that doesn't split up a tightly-bound operator is:
    ///
    /// (1 + 2) + 3
    ///
    /// This situation could be specified with a precedence of (n, n + 1), for example.
    let withInfix<'tokenTag, 'token, 'expr when 'tokenTag : comparison>
        (tokenType : 'tokenTag)
        (precedence : int * int)
        (construct : 'expr -> 'expr -> 'expr)
        (parser : Parser<'tokenTag, 'token, 'expr>)
        : Parser<'tokenTag, 'token, 'expr>
        =
        { parser with
            Infix =
                parser.Infix
                |> Map.change
                    tokenType
                    (fun existing ->
                        match existing with
                        | None -> Some (precedence, construct)
                        | Some _ -> failwithf "Duplicate infix parser added for token %+A" tokenType
                    )
        }

    /// Add a bracket-like parser to the parser, introduced by a given delimiter.
    /// See the docs for BracketLikeParser.
    ///
    /// If you have multiple `BracketLikeParser`s, each with the same beginning delimiter,
    /// we will try them all, and return the valid one which had the most bracket-like tokens in.
    /// It's probably possible to create an ambiguous parse this way with an inappropriate grammar;
    /// if this happens while parsing, we throw.
    let withBracketLike<'tokenTag, 'token, 'expr when 'tokenTag : comparison>
        (tokenType : 'tokenTag)
        (toAdd : BracketLikeParser<'tokenTag, 'expr>)
        (parser : Parser<'tokenTag, 'token, 'expr>)
        : Parser<'tokenTag, 'token, 'expr>
        =
        { parser with
            BracketLike =
                parser.BracketLike
                |> Map.change
                    tokenType
                    (fun existing ->
                        match existing with
                        | None -> Some [ toAdd ]
                        | Some existing -> Some (toAdd :: existing)
                    )
        }

    let rec private parseBracketLike
        (parser : Parser<'tokenTag, 'token, 'expr>)
        (inputString : string)
        (subParsers : BracketLikeParser<'tokenTag, 'expr> list)
        (exprsSoFar : 'expr option list)
        (tokens : 'token list)
        : ('expr * 'token list) list
        =
        let subParsersEnded, subParsersContinuing =
            subParsers |> List.partition _.BoundaryTokens.IsEmpty

        let fromSubParsersContinuing =
            // If there are any subparsers expecting further boundary tokens,
            // then we should get the next contents so that we can provide it to them.
            // But if there aren't any such subparsers, we don't have to (and indeed
            // it would be incorrect to, because the token stream might validly have ended).
            if subParsersContinuing.IsEmpty then
                []
            else

            // Check if the next token is a boundary token (indicating an empty segment)
            let nextIsBoundary =
                match tokens with
                | [] -> false
                | next :: _ ->
                    subParsersContinuing
                    |> List.exists (fun sp ->
                        match sp.BoundaryTokens with
                        | [] -> failwith "logic error: continuing parser has empty boundary tokens"
                        | head :: _ -> head = parser.GetTag next
                    )

            if nextIsBoundary then
                // Empty segment - next token is a boundary
                match tokens with
                | [] -> failwith "logic error: nextIsBoundary was true but tokens is empty"
                | next :: rest ->

                // Which bracket-like parsers match this boundary token?
                let subParsersContinuing =
                    subParsersContinuing
                    |> List.choose (fun subParser ->
                        match subParser.BoundaryTokens with
                        | [] -> failwith "logic error, this was ruled out earlier"
                        | head :: boundary ->
                            if head = parser.GetTag next then
                                Some
                                    { subParser with
                                        BoundaryTokens = boundary
                                    }
                            else
                                None
                    )

                // Proceed with None for this empty segment
                parseBracketLike parser inputString subParsersContinuing (None :: exprsSoFar) rest
            else
                // Non-empty segment - parse the content
                let contents, rest = parseInner parser inputString tokens 0

                match rest with
                | [] ->
                    // No valid parses down this path: we've run out of tokens despite all
                    // bracket-like parsers expecting another boundary token
                    []
                | next :: rest ->

                // Which bracket-like parsers are now ruled out by the next bracket-like token?
                let subParsersContinuing =
                    subParsersContinuing
                    |> List.choose (fun subParser ->
                        match subParser.BoundaryTokens with
                        | [] -> failwith "logic error, this was ruled out earlier"
                        | head :: boundary ->
                            if head = parser.GetTag next then
                                Some
                                    { subParser with
                                        BoundaryTokens = boundary
                                    }
                            else
                                None
                    )

                // And proceed with the ones which are still valid.
                parseBracketLike parser inputString subParsersContinuing (Some contents :: exprsSoFar) rest

        // We'll only consider bracket-like parsers which have already consumed all they want to consume
        // if no other parser wanted to consume more. (That is, `if-then-else` is preferred to `if-then`
        // as long as `if-then-else` succeeded, but if `if-then-else` failed, we'll fall back to `if-then`.)
        if fromSubParsersContinuing.IsEmpty then
            subParsersEnded
            |> List.map (fun subParser ->
                if subParser.ConsumeAfterFinalToken then
                    // Check if the final segment is empty
                    match tokens with
                    | [] ->
                        // Empty final segment
                        subParser.Construct (List.rev (None :: exprsSoFar)), tokens
                    | _ ->
                        // Non-empty final segment
                        let contents, rest = parseInner parser inputString tokens 0
                        subParser.Construct (List.rev (Some contents :: exprsSoFar)), rest
                else
                    subParser.Construct (List.rev exprsSoFar), tokens
            )
        else
            fromSubParsersContinuing

    /// The input string is only required so that the tokens have something to slice into.
    and private parseInner
        (parser : Parser<'tokenTag, 'token, 'expr>)
        (inputString : string)
        (tokens : 'token list)
        (minBinding : int)
        : 'expr * 'token list
        =
        match tokens with
        | [] -> failwith "cannot parse an empty list of tokens"
        | firstToken :: rest ->

        let lhs, rest =
            match parser.Atom inputString firstToken with
            | Some token -> token, rest
            | None ->

            match Map.tryFind (parser.GetTag firstToken) parser.BracketLike with
            | Some parse ->
                // This is an ambiguous parse if multiple parsers genuinely matched.
                // (We already filter to the longest possible matching parser.)
                match parseBracketLike parser inputString parse [] rest with
                | [] -> failwithf "Failed to parse any bracket-like parsers for %+A" firstToken
                | [ x ] -> x
                | _ ->
                    failwithf
                        "Ambiguous parse for bracket-like construct. You should restrict the grammar. %+A"
                        firstToken
            | None ->

            match Map.tryFind (parser.GetTag firstToken) parser.UnaryPrefix with
            | Some (((), precedence), assemble) ->
                let rhs, rest = parseInner parser inputString rest precedence
                assemble rhs, rest
            | None -> failwithf "didn't get an atom or prefix, got: %+A" firstToken

        let rec go (lhs : 'expr) (tokens : 'token list) : 'expr * 'token list =
            match tokens with
            | [] -> lhs, []
            | op :: rest ->

            let fromBracketed =
                match Map.tryFind (parser.GetTag op) parser.BracketLike with
                | Some parse ->
                    let parse = parse |> List.filter _.ConsumeBeforeInitialToken

                    match parseBracketLike parser inputString parse [ Some lhs ] rest with
                    | [ result ] -> Some result
                    | _ :: _ -> failwithf "Ambiguous parse (multiple matches) at token %+A" op
                    | [] -> None
                | None -> None

            match fromBracketed with
            | Some (lhs, rest) -> go lhs rest
            | None ->

            match Map.tryFind (parser.GetTag op) parser.UnaryPostfix with
            | Some ((precedence, ()), construct) ->
                if precedence < minBinding then
                    lhs, op :: rest
                else
                    go (construct lhs) rest
            | None ->

            match Map.tryFind (parser.GetTag op) parser.Infix with
            | Some ((leftBinding, rightBinding), construct) ->
                if leftBinding < minBinding then
                    lhs, op :: rest
                else

                let rhs, remainingTokens = parseInner parser inputString rest rightBinding

                go (construct lhs rhs) remainingTokens
            | None ->
                // TODO: This could be function application!
                lhs, op :: rest

        go lhs rest

    /// Execute the given parser against a string which was tokenised in the given way.
    /// We give you the string so that you may have your tokens slice into it.
    ///
    /// Returns the parsed expression, and any leftover tokens that may be trailing.
    let execute<'tokenTag, 'token, 'expr when 'tokenTag : comparison>
        (parser : Parser<'tokenTag, 'token, 'expr>)
        (inputString : string)
        (tokens : 'token list)
        : 'expr * 'token list
        =
        parseInner parser inputString tokens 0
