# WoofWare.PrattParser

![Project logo: the face of a cartoon Shiba Inu wearing glasses, looking directly at the viewer, with a background of cartoon basic mathematical symbols. The dog is holding a pencil and a notepad with more symbols written on it. At the bottom is the slogan "EXPRESSION PAWS-ER".](./PrattParser/logo.png)

A [Pratt parser](https://langdev.stackexchange.com/q/3254/1025), based on [Matklad's tutorial](https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html).

Bug reports welcome; I wouldn't exactly say this is well-tested, although it has worked correctly on the two things I've used it for so far.

See [the example](PrattParser.Example/) for how you should use this.
In brief:

* [Define a lexer somehow](PrattParser.Example/Lexer.fs) (which produces a stream of `Token`s, where a `Token` is [a type you define](PrattParser.Example/Domain.fs)).
* Define what it means for a lexeme to be *atomic* (this is [`Expr.atom`](PrattParser.Example/Example.fs)). Atomic tokens can appear on their own to form an expression all by themselves, or they can be combined or modified using operations specified by non-atomic tokens.
* Define how the various non-atomic lexemes behave (this is `Parser.withUnaryPrefix` and friends) to combine subexpressions into larger expressions.

We supply:

* `Parser.withUnaryPrefix`, which specifies e.g. that the token `!` can appear as a unary prefix of another expression, and that when it does, it indicates (e.g.) negation.
* `Parser.withUnaryPostfix`, which specifies e.g. that the token `!` can appear as a unary suffix of another expression, and that when it does, it indicates (e.g.) the factorial.
* `Parser.withInfix`, which specifies e.g. that the token `+` can appear between two expressions, and that when it does, it indicates (e.g.) addition.
* `Parser.withBracketLike`, which specifies e.g. that the token `(` can appear before an expression, and that it's terminated by e.g. `)`. You can also implement `if/then/else` this way: that's just a weird kind of bracket and comma (and the `else` is kind of mixfix: it needs to consume one expression *after* itself, by contrast with the closing bracket `)` which does not).

When specifying how expressions combine, you also provide precedences; numerically larger precedences are higher-precedence (so they bind more tightly).
Precedences are given as pairs, so that you can define e.g. left-associativity (resp. right-associativity) by having the infix operator bind more strongly to the right (resp. left).
For example, if `+` has precedence (10, 5), so it binds strongly on the left and weakly on the right:

* `a + b + c` is morally `a+ b+ c` because the `+` binds strongly to the left;
* `a+ b+ c` can only be bracketed as `a+ (b+ c)` because the rightmost `+` wants to stick to the `b` more than it wants to stick to the `c`;
* so `a + b + c` is equal to `a + (b + c)`, i.e. `+` is right-associative.
