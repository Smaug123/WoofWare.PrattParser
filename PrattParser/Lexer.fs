namespace PrattParser

[<RequireQualifiedAccess>]
module Lexer =
    let private (|Digit|_|) (c : char) : byte option =
        if '0' <= c && c <= '9' then
            Some (byte c - byte '0')
        else
            None

    let lex (s : string) : Token seq =
        seq {
            let mutable i = 0

            while i < s.Length do
                match i, s.[i] with
                | Token.SingleChar token ->
                    i <- i + 1
                    yield token
                | startI, Digit _ ->
                    i <- i + 1
                    let mutable shouldBreak = false

                    while not shouldBreak do
                        match s.[i] with
                        | Digit _ -> i <- i + 1
                        | _ -> shouldBreak <- true

                    yield Token.standalone TokenType.ConstInt startI (i - startI)
                | _, ' ' -> i <- i + 1
                | startI, c ->
                    if ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z') then
                        i <- i + 1
                        let mutable shouldBreak = false

                        while not shouldBreak do
                            let c = s.[i]

                            if ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z') || ('0' <= c && c <= '9') then
                                i <- i + 1
                            else
                                shouldBreak <- true

                        yield Token.standalone TokenType.Var startI (i - startI)
                    else
                        failwithf "Could not tokenize, char %c, at position %i" c startI
        }
