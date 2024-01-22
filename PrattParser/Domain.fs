namespace PrattParser

[<RequireQualifiedAccess>]
type Expr =
    | Plus of Expr * Expr
    | Times of Expr * Expr
    | UnaryMinus of Expr
    | Minus of Expr * Expr
    | Int of int
    | FunctionCall of Expr * Expr
    | Var of string
    | Factorial of Expr
    | Paren of Expr
    | IfThenElse of Expr * Expr * Expr
    | IfThen of Expr * Expr

[<RequireQualifiedAccess>]
module Expr =
    let plus a b = Expr.Plus (a, b)
    let times a b = Expr.Times (a, b)
    let unaryMinus a = Expr.UnaryMinus a
    let minus a b = Expr.Minus (a, b)
    let constInt a = Expr.Int a
    let functionCall f x = Expr.FunctionCall (f, x)
    let var name = Expr.Var name
    let factorial a = Expr.Factorial a
    let paren a = Expr.Paren a

    let ifThenElse ifClause thenClause elseClause =
        Expr.IfThenElse (ifClause, thenClause, elseClause)

    let ifThen ifClause thenClause = Expr.IfThen (ifClause, thenClause)

[<RequireQualifiedAccess>]
type TokenType =
    | Plus
    | Minus
    | Times
    | ConstInt
    | LeftBracket
    | RightBracket
    | Var
    | Factorial
    | If
    | Then
    | Else

type Token =
    {
        Type : TokenType
        /// The token is represented in the string as s.[left .. left + len], i.e. inclusive.
        Trivia : int * int
    }

[<RequireQualifiedAccess>]
module Token =
    let standalone (ty : TokenType) (left : int) (len : int) =
        {
            Type = ty
            Trivia = (left, len)
        }

    let standalone' (ty : TokenType) (singleCharPos : int) =
        {
            Type = ty
            Trivia = (singleCharPos, 1)
        }

    let (|SingleChar|_|) (i : int, c : char) : Token option =
        match c with
        | '(' -> standalone' TokenType.LeftBracket i |> Some
        | ')' -> standalone' TokenType.RightBracket i |> Some
        | '*' -> standalone' TokenType.Times i |> Some
        | '+' -> standalone' TokenType.Plus i |> Some
        | '-' -> standalone' TokenType.Minus i |> Some
        | '!' -> standalone' TokenType.Factorial i |> Some
        | _ -> None
