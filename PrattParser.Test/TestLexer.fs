namespace PrattParser.Test

open PrattParser
open NUnit.Framework
open FsUnitTyped

[<TestFixture>]
module TestLexer =

    [<Test>]
    let ``Lexer looks plausible`` () =
        let input = "g x y + a * func (b + 100)"

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
                    Type = TokenType.Plus
                    Trivia = (6, 1)
                }
                {
                    Type = TokenType.Var
                    Trivia = (8, 1)
                }
                {
                    Type = TokenType.Times
                    Trivia = (10, 1)
                }
                {
                    Type = TokenType.Var
                    Trivia = (12, 4)
                }
                {
                    Type = TokenType.LeftBracket
                    Trivia = (17, 1)
                }
                {
                    Type = TokenType.Var
                    Trivia = (18, 1)
                }
                {
                    Type = TokenType.Plus
                    Trivia = (20, 1)
                }
                {
                    Type = TokenType.ConstInt
                    Trivia = (22, 3)
                }
                {
                    Type = TokenType.RightBracket
                    Trivia = (25, 1)
                }
            ]

        Lexer.lex input |> shouldEqual expected
