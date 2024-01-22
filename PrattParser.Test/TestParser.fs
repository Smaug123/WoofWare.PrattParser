namespace PrattParser.Test

open PrattParser
open NUnit.Framework
open FsUnitTyped

[<TestFixture>]
module TestParser =

    let parserTestCases =
        [
            "1", Expr.constInt 1
            "a", Expr.var "a"
            "-1", Expr.unaryMinus (Expr.constInt 1)
            "-a", Expr.unaryMinus (Expr.var "a")
            "-a!", Expr.unaryMinus (Expr.factorial (Expr.var "a"))
            "-a! + b", Expr.plus (Expr.unaryMinus (Expr.factorial (Expr.var "a"))) (Expr.var "b")
            "(-a)! + b", Expr.plus (Expr.factorial (Expr.paren (Expr.unaryMinus (Expr.var "a")))) (Expr.var "b")
            "if x then y else z", Expr.ifThenElse (Expr.var "x") (Expr.var "y") (Expr.var "z")
            "if x then y", Expr.ifThen (Expr.var "x") (Expr.var "y")
            "1 + if x then y", Expr.plus (Expr.constInt 1) (Expr.ifThen (Expr.var "x") (Expr.var "y"))
            "if x + 1 then y else z + 3",
            Expr.ifThenElse
                (Expr.plus (Expr.var "x") (Expr.constInt 1))
                (Expr.var "y")
                (Expr.plus (Expr.var "z") (Expr.constInt 3))
            "(if x + 1 then y else z) + 3",
            Expr.plus
                (Expr.paren (Expr.ifThenElse (Expr.plus (Expr.var "x") (Expr.constInt 1)) (Expr.var "y") (Expr.var "z")))
                (Expr.constInt 3)

            "g x y + a * (func b c)",
            let gXY =
                Expr.functionCall (Expr.functionCall (Expr.var "g") (Expr.var "x")) (Expr.var "y")

            let fAPlusB =
                Expr.functionCall (Expr.Var "func") (Expr.plus (Expr.Var "b") (Expr.Var "c"))

            Expr.plus gXY (Expr.times (Expr.Var "a") fAPlusB)

        ]
        |> List.map TestCaseData

    [<TestCaseSource(nameof parserTestCases)>]
    let ``Parser looks plausible`` (input : string, expected : Expr) =
        let tokens = Lexer.lex input |> List.ofSeq

        let expr, remaining = Parser.parse input tokens
        expr |> shouldEqual expected
        remaining |> shouldEqual []
