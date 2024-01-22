namespace PrattParser

open System
open System.Globalization

[<RequireQualifiedAccess>]
module Parser =
    let atom (inputString : string) (token : Token) : Expr option =
        match token.Type with
        | TokenType.ConstInt ->
            let start, len = token.Trivia

            Some (
                Expr.constInt (
                    Int32.Parse (
                        inputString.AsSpan().Slice (start, len),
                        NumberStyles.None,
                        CultureInfo.InvariantCulture
                    )
                )
            )
        | TokenType.Var ->
            let start, len = token.Trivia
            Some (Expr.var (inputString.Substring (start, len)))
        | TokenType.Plus
        | TokenType.Minus
        | TokenType.Times
        | TokenType.Factorial
        | TokenType.If
        | TokenType.Then
        | TokenType.Else
        | TokenType.LeftBracket
        | TokenType.RightBracket -> None

    let buildBinary (op : TokenType) (lhs : Expr) (rhs : Expr) : Expr =
        match op with
        | TokenType.Plus -> Expr.plus lhs rhs
        | TokenType.Minus -> Expr.minus lhs rhs
        | TokenType.Times -> Expr.times lhs rhs
        | _ -> failwithf "unexpected operation %+A seems not to be a binary op" op

    let buildUnary (op : TokenType) (expr : Expr) : Expr =
        match op with
        | TokenType.Plus -> expr
        | TokenType.Minus -> Expr.unaryMinus expr
        | TokenType.Factorial -> Expr.factorial expr
        | _ -> failwithf "not a prefix op: %+A" op

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
            Unary : Map<TokenType, (unit * int) * (Expr -> Expr)>
            Atom : string -> Token -> Expr option
            BracketLike : Map<TokenType, BracketLikeParser list>
        }

    let basicParser : Parser =
        {
            Atom = atom
            Unary =
                [ TokenType.Plus, (((), 5), id) ; TokenType.Minus, (((), 5), Expr.unaryMinus) ]
                |> Map.ofList
            BracketLike =
                [
                    TokenType.LeftBracket,
                    [
                        {
                            ConsumeAfterFinalToken = false
                            BoundaryTokens = [ TokenType.RightBracket ]
                            Construct = Seq.exactlyOne >> Expr.paren
                        }
                    ]
                    TokenType.If,
                    [
                        {
                            ConsumeAfterFinalToken = true
                            BoundaryTokens = [ TokenType.Then ; TokenType.Else ]
                            Construct =
                                fun s ->
                                    match s with
                                    | [ ifClause ; thenClause ; elseClause ] ->
                                        Expr.ifThenElse ifClause thenClause elseClause
                                    | _ -> failwith "logic error"
                        }
                        {
                            ConsumeAfterFinalToken = true
                            BoundaryTokens = [ TokenType.Then ]
                            Construct =
                                fun s ->
                                    match s with
                                    | [ ifClause ; thenClause ] -> Expr.ifThen ifClause thenClause
                                    | _ -> failwith "logic error"
                        }
                    ]
                ]
                |> Map.ofList
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

    /// The input string is only required so that the tokens
    /// have something to slice into.
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
            | true, parse -> parseBracketLike parser inputString parse [] rest |> List.exactlyOne
            | false, _ ->

            match parser.Unary.TryGetValue firstToken.Type with
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

            match Token.postfixPrecedence op.Type with
            | Some (precedence, ()) ->
                if precedence < minBinding then
                    printfn "Hit a postfix op which does not bind: %+A" op
                    lhs, rest
                else
                    printfn "Hit a postfix op which binds: %+A" op
                    go (buildUnary op.Type lhs) rest
            | None ->

            match Token.infixPrecedence op.Type with
            | None -> lhs, op :: rest
            | Some (leftBinding, rightBinding) ->

            if leftBinding < minBinding then
                printfn "Hit an infix op which does not bind on the left: %+A" op
                lhs, op :: rest
            else

            printfn "Hit an infix op which binds on the left: %+A" op

            let rhs, remainingTokens = parseInner parser inputString rest rightBinding

            go (buildBinary op.Type lhs rhs) remainingTokens

        go lhs rest

    let parse parser inputString tokens = parseInner parser inputString tokens 0
