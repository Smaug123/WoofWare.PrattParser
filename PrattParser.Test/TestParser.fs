namespace PrattParser.Test

open ParseExample
open PrattParser
open NUnit.Framework
open FsUnitTyped

[<TestFixture>]
module TestParser =

    let parserTestCases =
        [
            "1", Expr.constInt 1
            "1 + 2 * 3", Expr.plus (Expr.constInt 1) (Expr.times (Expr.constInt 2) (Expr.constInt 3))
            "a + b * c * d + e",
            Expr.plus
                (Expr.plus (Expr.var "a") (Expr.times (Expr.times (Expr.var "b") (Expr.var "c")) (Expr.var "d")))
                (Expr.var "e")
            "--1 * 2", Expr.times (Expr.unaryMinus (Expr.unaryMinus (Expr.constInt 1))) (Expr.constInt 2)
            "-9!", Expr.unaryMinus (Expr.factorial (Expr.constInt 9))
            "(((0)))", Expr.paren (Expr.paren (Expr.paren (Expr.constInt 0)))
            "x.[0].[1]", Expr.arrayIndex (Expr.arrayIndex (Expr.var "x") (Expr.constInt 0)) (Expr.constInt 1)
            "if a = 0 then b else c = d",
            Expr.ifThenElse
                (Expr.equal (Expr.var "a") (Expr.constInt 0))
                (Expr.var "b")
                (Expr.equal (Expr.var "c") (Expr.var "d"))
            "a", Expr.var "a"
            "-1", Expr.unaryMinus (Expr.constInt 1)
            "-a", Expr.unaryMinus (Expr.var "a")
            "-a!", Expr.unaryMinus (Expr.factorial (Expr.var "a"))
            "-a! + b", Expr.plus (Expr.unaryMinus (Expr.factorial (Expr.var "a"))) (Expr.var "b")
            "(-a)! + b", Expr.plus (Expr.factorial (Expr.paren (Expr.unaryMinus (Expr.var "a")))) (Expr.var "b")
            "if x then y else z", Expr.ifThenElse (Expr.var "x") (Expr.var "y") (Expr.var "z")
            "if x then y", Expr.ifThen (Expr.var "x") (Expr.var "y")
            "1 + if x then y", Expr.plus (Expr.constInt 1) (Expr.ifThen (Expr.var "x") (Expr.var "y"))
            "if x then y else if r then s",
            Expr.ifThenElse (Expr.var "x") (Expr.var "y") (Expr.ifThen (Expr.var "r") (Expr.var "s"))
            "if x then y else if r then s else 5",
            Expr.ifThenElse
                (Expr.var "x")
                (Expr.var "y")
                (Expr.ifThenElse (Expr.var "r") (Expr.var "s") (Expr.constInt 5))
            "if if x then y else z then a",
            Expr.ifThen (Expr.ifThenElse (Expr.var "x") (Expr.var "y") (Expr.var "z")) (Expr.var "a")
            "if if x then y else z then a else b",
            Expr.ifThenElse (Expr.ifThenElse (Expr.var "x") (Expr.var "y") (Expr.var "z")) (Expr.var "a") (Expr.var "b")
            "if x + 1 then y else z + 3",
            Expr.ifThenElse
                (Expr.plus (Expr.var "x") (Expr.constInt 1))
                (Expr.var "y")
                (Expr.plus (Expr.var "z") (Expr.constInt 3))
            "if x + 1 then y else z!",
            Expr.ifThenElse (Expr.plus (Expr.var "x") (Expr.constInt 1)) (Expr.var "y") (Expr.factorial (Expr.var "z"))
            "(if x + 1 then y else z) + 3",
            Expr.plus
                (Expr.paren (Expr.ifThenElse (Expr.plus (Expr.var "x") (Expr.constInt 1)) (Expr.var "y") (Expr.var "z")))
                (Expr.constInt 3)

        (*
        // TODO: implement
            "g x y + a * (func b c)",
            let gXY =
                Expr.functionCall (Expr.functionCall (Expr.var "g") (Expr.var "x")) (Expr.var "y")

            let fAPlusB =
                Expr.functionCall (Expr.Var "func") (Expr.plus (Expr.Var "b") (Expr.Var "c"))

            Expr.plus gXY (Expr.times (Expr.Var "a") fAPlusB)
*)

        ]
        |> List.map TestCaseData

    [<TestCaseSource(nameof parserTestCases)>]
    let ``Parser looks plausible`` (input : string, expected : Expr) =
        let tokens = Lexer.lex input |> List.ofSeq

        let expr, remaining = Parser.execute Example.parser input tokens
        remaining |> shouldEqual []
        expr |> shouldEqual expected
