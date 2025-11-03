namespace WoofWare.PrattParser.Test

open ParseExample
open NUnit.Framework
open FsUnitTyped

[<TestFixture>]
module TestLexer =

    [<Test>]
    let ``Lexer looks plausible`` () =
        let input = "g x y! + a * func (b + 100)"

        let expected =
            [
                {
                    Type = TokenType.Var
                    Trivia = (0, 1)
                }
                {
                    Type = TokenType.Var
                    Trivia = (2, 1)
                }
                {
                    Type = TokenType.Var
                    Trivia = (4, 1)
                }
                {
                    Type = TokenType.Factorial
                    Trivia = (5, 1)
                }
                {
                    Type = TokenType.Plus
                    Trivia = (7, 1)
                }
                {
                    Type = TokenType.Var
                    Trivia = (9, 1)
                }
                {
                    Type = TokenType.Times
                    Trivia = (11, 1)
                }
                {
                    Type = TokenType.Var
                    Trivia = (13, 4)
                }
                {
                    Type = TokenType.LeftBracket
                    Trivia = (18, 1)
                }
                {
                    Type = TokenType.Var
                    Trivia = (19, 1)
                }
                {
                    Type = TokenType.Plus
                    Trivia = (21, 1)
                }
                {
                    Type = TokenType.ConstInt
                    Trivia = (23, 3)
                }
                {
                    Type = TokenType.RightBracket
                    Trivia = (26, 1)
                }
            ]

        Lexer.lex input |> Seq.toList |> shouldEqual expected

    [<Test>]
    let ``Bug 2: Variables starting with keywords should tokenize as single variables`` () =
        // This test demonstrates Bug 2: the lexer incorrectly splits variables
        // that start with keywords ("if", "then", "else") into multiple tokens.
        // See BUGS.md for details.

        let input = "iffy"

        let expected =
            [
                {
                    Type = TokenType.Var
                    Trivia = (0, 4)
                }
            ]

        // Currently fails: lexer produces [If, Var("fy")] instead of [Var("iffy")]
        Lexer.lex input |> Seq.toList |> shouldEqual expected
