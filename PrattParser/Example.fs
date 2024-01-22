namespace PrattParser

open System
open System.Globalization

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
        | TokenType.Factorial
        | TokenType.If
        | TokenType.Then
        | TokenType.Else
        | TokenType.LeftBracket
        | TokenType.RightBracket -> None

    let parser =
        Parser.empty _.Type
        |> Parser.defineAtoms atom
        |> Parser.withUnaryPostfix TokenType.Factorial (7, ()) Expr.factorial
        |> Parser.withUnaryPrefix TokenType.Plus ((), 5) id
        |> Parser.withUnaryPrefix TokenType.Minus ((), 5) Expr.unaryMinus
        |> Parser.withInfix TokenType.Plus (1, 2) Expr.plus
        |> Parser.withInfix TokenType.Minus (1, 2) Expr.minus
        |> Parser.withInfix TokenType.Times (1, 2) Expr.times
        |> Parser.withBracketLike
            TokenType.LeftBracket
            {
                ConsumeAfterFinalToken = false
                BoundaryTokens = [ TokenType.RightBracket ]
                Construct = Seq.exactlyOne >> Expr.paren
            }
        |> Parser.withBracketLike
            TokenType.If
            {
                ConsumeAfterFinalToken = true
                BoundaryTokens = [ TokenType.Then ; TokenType.Else ]
                Construct =
                    fun s ->
                        match s with
                        | [ ifClause ; thenClause ; elseClause ] -> Expr.ifThenElse ifClause thenClause elseClause
                        | _ -> failwith "logic error"
            }
        |> Parser.withBracketLike
            TokenType.If
            {
                ConsumeAfterFinalToken = true
                BoundaryTokens = [ TokenType.Then ]
                Construct =
                    fun s ->
                        match s with
                        | [ ifClause ; thenClause ] -> Expr.ifThen ifClause thenClause
                        | _ -> failwith "logic error"
            }
