namespace ParseExample

open System
open System.Globalization
open WoofWare.PrattParser

[<RequireQualifiedAccess>]
module Example =
    let private atom (inputString : string) (token : Token) : Expr option =
        match token.Type with
        | TokenType.ConstInt ->
            let start, len = token.Trivia

            Int32.Parse (inputString.AsSpan().Slice (start, len), NumberStyles.None, CultureInfo.InvariantCulture)
            |> Expr.constInt
            |> Some
        | TokenType.Var ->
            let start, len = token.Trivia
            Some (Expr.var (inputString.Substring (start, len)))
        | TokenType.Plus
        | TokenType.Minus
        | TokenType.Times
        | TokenType.Equal
        | TokenType.Factorial
        | TokenType.If
        | TokenType.Then
        | TokenType.Else
        | TokenType.ArrayIndex
        | TokenType.RightSquareBracket
        | TokenType.LeftBracket
        | TokenType.RightBracket -> None

    let parser =
        Parser.make (fun token -> token.Type) atom
        |> Parser.withUnaryPostfix TokenType.Factorial (11, ()) Expr.factorial
        |> Parser.withUnaryPrefix TokenType.Plus ((), 9) id
        |> Parser.withUnaryPrefix TokenType.Minus ((), 9) Expr.unaryMinus
        |> Parser.withInfix TokenType.Plus (5, 6) Expr.plus
        |> Parser.withInfix TokenType.Minus (5, 6) Expr.minus
        |> Parser.withInfix TokenType.Times (7, 8) Expr.times
        |> Parser.withInfix TokenType.Equal (2, 1) Expr.equal
        |> Parser.withBracketLike
            TokenType.LeftBracket
            {
                ConsumeBeforeInitialToken = false
                ConsumeAfterFinalToken = false
                BoundaryTokens = [ TokenType.RightBracket ]
                Construct =
                    fun s ->
                        match s with
                        | [ Some expr ] -> Expr.paren expr
                        | [ None ] -> failwith "Empty parentheses are not supported"
                        | _ -> failwith "logic error"
            }
        |> Parser.withBracketLike
            TokenType.If
            {
                ConsumeBeforeInitialToken = false
                ConsumeAfterFinalToken = true
                BoundaryTokens = [ TokenType.Then ; TokenType.Else ]
                Construct =
                    fun s ->
                        match s with
                        | [ Some ifClause ; Some thenClause ; Some elseClause ] ->
                            Expr.ifThenElse ifClause thenClause elseClause
                        | [ None ; _ ; _ ] -> failwith "Empty if condition is not supported"
                        | [ _ ; None ; _ ] -> failwith "Empty then clause is not supported"
                        | [ _ ; _ ; None ] -> failwith "Empty else clause is not supported"
                        | _ -> failwith "logic error"
            }
        |> Parser.withBracketLike
            TokenType.If
            {
                ConsumeBeforeInitialToken = false
                ConsumeAfterFinalToken = true
                BoundaryTokens = [ TokenType.Then ]
                Construct =
                    fun s ->
                        match s with
                        | [ Some ifClause ; Some thenClause ] -> Expr.ifThen ifClause thenClause
                        | [ None ; _ ] -> failwith "Empty if condition is not supported"
                        | [ _ ; None ] -> failwith "Empty then clause is not supported"
                        | _ -> failwith "logic error"
            }
        |> Parser.withBracketLike
            TokenType.ArrayIndex
            {
                ConsumeBeforeInitialToken = true
                ConsumeAfterFinalToken = false
                BoundaryTokens = [ TokenType.RightSquareBracket ]
                Construct =
                    fun s ->
                        match s with
                        | [ Some arg ; Some contents ] -> Expr.arrayIndex arg contents
                        | [ None ; _ ] -> failwith "Empty array target is not supported"
                        | [ _ ; None ] -> failwith "Empty array index is not supported"
                        | _ -> failwith "logic error"
            }
