namespace PrattParser

open System
open System.Globalization

type BracketLikeParser =
    {
        /// Whether to consume input after the final token, e.g. like `if...then...else...` consumes,
        /// whereas `(...)` does not
        ConsumeAfterFinalToken : bool
        BoundaryTokens : TokenType list
        Construct : Expr list -> Expr
    }

type Parser =
    {
        UnaryPrefix : Map<TokenType, (unit * int) * (Expr -> Expr)>
        UnaryPostfix : Map<TokenType, (int * unit) * (Expr -> Expr)>
        Infix : Map<TokenType, (int * int) * (Expr -> Expr -> Expr)>
        Atom : string -> Token -> Expr option
        BracketLike : Map<TokenType, BracketLikeParser list>
    }

[<RequireQualifiedAccess>]
module Parser =
    let empty =
        {
            UnaryPrefix = Map.empty
            UnaryPostfix = Map.empty
            Infix = Map.empty
            Atom = fun _ _ -> None
            BracketLike = Map.empty
        }

    let withUnaryPrefix (tokenType : TokenType) (precedence : unit * int) (construct : Expr -> Expr) (parser : Parser) =
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

    let withUnaryPostfix
        (tokenType : TokenType)
        (precedence : int * unit)
        (construct : Expr -> Expr)
        (parser : Parser)
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

    let withInfix
        (tokenType : TokenType)
        (precedence : int * int)
        (construct : Expr -> Expr -> Expr)
        (parser : Parser)
        : Parser
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

    let withBracketLike (tokenType : TokenType) (toAdd : BracketLikeParser) (parser : Parser) : Parser =
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

    let defineAtoms (atom : string -> Token -> Expr option) (parser : Parser) : Parser =
        { parser with
            Atom = atom
        }

    let rec parseBracketLike
        (parser : Parser)
        (inputString : string)
        (subParsers : BracketLikeParser list)
        (exprsSoFar : Expr list)
        (tokens : Token list)
        : (Expr * Token list) list
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
                        if head = next.Type then
                            Some
                                { subParser with
                                    BoundaryTokens = boundary
                                }
                        else
                            None
                )

            // And proceed with the ones which are still valid.
            parseBracketLike parser inputString subParsersContinuing (contents :: exprsSoFar) rest

        // We'll only consider bracket-like parsers which have already consumed all they want to consume
        // if no other parser wanted to consume more. (That is, `if-then-else` is preferred to `if-then`
        // as long as `if-then-else` succeeded, but if `if-then-else` failed, we'll fall back to `if-then`.)
        if fromSubParsersContinuing.IsEmpty then
            subParsersEnded
            |> List.map (fun subParser ->
                if subParser.ConsumeAfterFinalToken then
                    let contents, rest = parseInner parser inputString tokens 0
                    subParser.Construct (List.rev (contents :: exprsSoFar)), rest
                else
                    subParser.Construct (List.rev exprsSoFar), tokens
            )
        else
            fromSubParsersContinuing

    /// The input string is only required so that the tokens have something to slice into.
    and parseInner
        (parser : Parser)
        (inputString : string)
        (tokens : Token list)
        (minBinding : int)
        : Expr * Token list
        =
        match tokens with
        | [] -> failwith "cannot parse an empty list of tokens"
        | firstToken :: rest ->

        let lhs, rest =
            match parser.Atom inputString firstToken with
            | Some token ->
                printfn "Parsed an atom: %+A" token
                token, rest
            | None ->

            match parser.BracketLike.TryGetValue firstToken.Type with
            | true, parse ->
                // This is an ambiguous parse if multiple parsers genuinely matched.
                // (We already filter to the longest possible matching parser.)
                match parseBracketLike parser inputString parse [] rest with
                | [] -> failwithf "Failed to parse any bracket-like parsers for %+A" firstToken
                | [ x ] -> x
                | _ ->
                    failwithf
                        "Ambiguous parse for bracket-like construct. You should restrict the grammar. %+A"
                        firstToken
            | false, _ ->

            match parser.UnaryPrefix.TryGetValue firstToken.Type with
            | true, (((), precedence), assemble) ->
                printfn "Parsing a prefix op: %+A" firstToken
                let rhs, rest = parseInner parser inputString rest precedence
                printfn "Returning to parse of prefix op: %+A, remaining tokens: %+A" firstToken rest
                assemble rhs, rest
            | false, _ -> failwithf "didn't get an atom or prefix, got: %+A" firstToken

        let rec go (lhs : Expr) (tokens : Token list) : Expr * Token list =
            match tokens with
            | [] -> lhs, []
            | op :: rest ->

            match parser.UnaryPostfix.TryGetValue op.Type with
            | true, ((precedence, ()), construct) ->
                if precedence < minBinding then
                    printfn "Hit a postfix op which does not bind: %+A" op
                    lhs, rest
                else
                    printfn "Hit a postfix op which binds: %+A" op
                    go (construct lhs) rest
            | false, _ ->

            match parser.Infix.TryGetValue op.Type with
            | true, ((leftBinding, rightBinding), construct) ->
                if leftBinding < minBinding then
                    printfn "Hit an infix op which does not bind on the left: %+A" op
                    lhs, op :: rest
                else

                printfn "Hit an infix op which binds on the left: %+A" op

                let rhs, remainingTokens = parseInner parser inputString rest rightBinding

                go (construct lhs rhs) remainingTokens
            | false, _ ->
                // TODO: This could be function application!
                lhs, op :: rest

        go lhs rest

    let parse parser inputString tokens = parseInner parser inputString tokens 0
