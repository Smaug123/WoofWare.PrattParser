namespace PrattParser

open System.Collections.Generic

type Parser<'parser> = 'parser -> Token -> Expr option

type ParserSpec<'parser> =
    private
        {
            Prefixes : (Token -> Parser<'parser> option) list
        }

[<RequireQualifiedAccess>]
module ParserSpec =
    let empty<'parser> () =
        {
            Prefixes = []
        }

    // TODO this is essentially duplicated which is a bit sad
    let private processPrefix (prefixOperation : Token) (operand : Expr) : Expr =
        match prefixOperation with
        | Token.Minus -> Expr.unaryMinus operand
        | token -> failwithf "logic error, this should not have happened: %+A, %+A" token operand

    let addPrefixParser<'parser> (input : Token) (spec : ParserSpec<'parser>) =
        let parser (token : Token) =
            if token = input then
                Some (fun parser processPrefix token )
            else
                None
        { spec with
            Prefixes = parser :: spec.Prefixes
        }

[<RequireQualifiedAccess>]
module Parse =

    /// Takes the current token.
    let prefixParser (parse : 'parser -> Expr) (toMatch : Token) : Parser<'parser> =
        fun parser token ->
            if token = toMatch then
                let operand = parse parser
                Some (processPrefix token operand)
            else None

    let varParser (parse : 'parser -> Expr) (toMatch : Token) : Parser<'parser> =
        fun _ token ->
            match token with
            | Token.Var x -> Some (Expr.Var x)
            | _ -> None

    let parserLookup (token : Token) (parser : ParserSpec<'parser>) : Parser<'parser> option =
        match parser.Prefixes token with
        | Some parser -> Some parser
        | None -> None

    let parserSpec =
        ParserSpec.empty ()
        |> ParserSpec.addPrefixParser Token.Minus

    let parseExpression (tokens : Token IEnumerator) : Expr =
        let token = tokens.MoveNext ()

module Program =

    [<EntryPoint>]
    let main argv =
        // g x y + a * f (b + c)
        let tokens =
            [
                Token.Var "g"
                Token.Var "x"
                Token.Var "y"
                Token.Plus
                Token.Var "a"
                Token.Times
                Token.Var "f"
                Token.LeftBracket
                Token.Var "b"
                Token.Plus
                Token.Var "c"
                Token.RightBracket
            ]

        let expected =
            let gXY =
                Expr.functionCall (Expr.functionCall (Expr.var "g") (Expr.var "x")) (Expr.var "y")

            let fAPlusB =
                Expr.functionCall (Expr.Var "f") (Expr.plus (Expr.Var "b") (Expr.Var "c"))

            Expr.plus gXY (Expr.times (Expr.Var "a") fAPlusB)

        0
