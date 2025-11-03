namespace ParseExample

[<RequireQualifiedAccess>]
module Lexer =
    let private (|Digit|_|) (c : char) : byte option =
        if '0' <= c && c <= '9' then
            Some (byte c - byte '0')
        else
            None

    let private isAlphaNumeric (c : char) : bool =
        ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z') || ('0' <= c && c <= '9')

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

                    while i < s.Length && not shouldBreak do
                        match s.[i] with
                        | Digit _ -> i <- i + 1
                        | _ -> shouldBreak <- true

                    yield Token.standalone TokenType.ConstInt startI (i - startI)
                | _, ' ' -> i <- i + 1
                | _, '.' when i < s.Length - 1 && s.[i + 1] = '[' ->
                    yield Token.standalone TokenType.ArrayIndex i 2
                    i <- i + 2
                | _, 'i' when
                    i < s.Length - 1
                    && s.[i + 1] = 'f'
                    && (i + 2 >= s.Length || not (isAlphaNumeric s.[i + 2]))
                    ->
                    yield Token.standalone TokenType.If i 2
                    i <- i + 2
                | _, 't' when
                    i < s.Length - 3
                    && s.[i + 1 .. i + 3] = "hen"
                    && (i + 4 >= s.Length || not (isAlphaNumeric s.[i + 4]))
                    ->
                    yield Token.standalone TokenType.Then i 4
                    i <- i + 4
                | _, 'e' when
                    i < s.Length - 3
                    && s.[i + 1 .. i + 3] = "lse"
                    && (i + 4 >= s.Length || not (isAlphaNumeric s.[i + 4]))
                    ->
                    yield Token.standalone TokenType.Else i 4
                    i <- i + 4
                | startI, c ->
                    if ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z') then
                        i <- i + 1
                        let mutable shouldBreak = false

                        while i < s.Length && not shouldBreak do
                            let c = s.[i]

                            if ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z') || ('0' <= c && c <= '9') then
                                i <- i + 1
                            else
                                shouldBreak <- true

                        yield Token.standalone TokenType.Var startI (i - startI)
                    else
                        failwithf "Could not tokenize, char %c, at position %i" c startI
        }
