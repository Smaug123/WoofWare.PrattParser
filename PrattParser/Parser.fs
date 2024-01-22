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

    /// The input string is only required so that the tokens
    /// have something to slice into.
    let rec parseInner (inputString : string) (tokens : Token list) (minBinding : int) : Expr * Token list =
        match tokens with
        | [] -> failwith "cannot parse an empty list of tokens"
        | firstToken :: rest ->

        let lhs, rest =
            match atom inputString firstToken with
            | Some token ->
                printfn "Parsed an atom: %+A" token
                token, rest
            | None ->

            if firstToken.Type = TokenType.LeftBracket then
                let contents, rest = parseInner inputString rest 0

                match rest with
                | [] -> failwith "unterminated bracket"
                | head :: _ when head.Type <> TokenType.RightBracket ->
                    failwithf "bracketed expression not followed by a right bracket, got: %+A" head
                | _ :: rest ->

                Expr.paren contents, rest

            else

            match Token.prefixPrecedence firstToken.Type with
            | Some ((), precedence) ->
                printfn "Parsing a prefix op: %+A" firstToken
                let rhs, rest = parseInner inputString rest precedence
                printfn "Returning to parse of prefix op: %+A, remaining tokens: %+A" firstToken rest
                buildUnary firstToken.Type rhs, rest
            | None -> failwithf "didn't get an atom or prefix, got: %+A" firstToken

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

            let rhs, remainingTokens = parseInner inputString rest rightBinding

            go (buildBinary op.Type lhs rhs) remainingTokens

        go lhs rest

    let parse inputString tokens = parseInner inputString tokens 0
