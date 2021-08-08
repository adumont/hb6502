# Homebrew 6502 SBC - FORTH

- [Homebrew 6502 SBC - FORTH](#homebrew-6502-sbc---forth)
- [Introduction](#introduction)
- [Try it!](#try-it)
  - [Examples](#examples)
- [References](#references)

# Introduction

This page is about my own implementation of FORTH for my Homebrew 6502 SBC.

![](./imgs/Forth.png)

# Try it!

You can use my Forth here [Alex Forth in Replit](https://replit.com/@AlexandreDumon1/Alex-Forth) (it might not be the latest version).  

Notice:
- It doesn't print {OK}
- It's not ANS Forth, but my own incomplete and free implementation.
- At this moment, it only supports 16 bit unsigned integers
- The base for all numbers is hexadecimal
- Words are case sensitive! (all default words defined in CAPS, for example SWAP, DUP, DROP... )
- There is NO stack underflow/overflow checking at all. So you can easily mess everything :). Don't worry. Just reset and start again!
- Android user, on mobile I find some keyboard mess with Repl.it. Try disabling all keyboard predictions and auto-typing features.

## Examples

### Hello world

```
S( Hello World!) TYPE CRLF
```

This should output this on a new line:

```
Hello world!
```

### Simple Arithmetics

```
1 2 +
```

This should output:

```
0003
```

### Variables

- Create variable Z:

```
VARIABLE Z
```

- Store value `AA01` into variable Z:

```
AA01
Z ! 
```

- Get value of variable Z and print it:

```
Z @ .
AA01 
```

### Memory Manipulation

Example 1: Read the reset vector:

```
FFFC @ .
```

This should output:

```
8000
```

### Define new words

Let's define the word `+!`. It takes a value and a variable name, and adds the value to the variable's value (and stores it back into the variable).

Let's enter:
```
: +! DUP @ ROT + SWAP ! ;
```
Now our FORTH knows this new word `+!`, so let's try it:

```
1 Z +!
Z @ .
```

Assuming `Z` was still at the value `AA01`, this should output:

```
AA02
```
### Fibonacci sequence

```
: next-fib   DUP ROT + ;

: fib DUP IF  0 1 ROT 1 DO next-fib LOOP SWAP  ELSE 0 THEN DROP ;

9 fib .
22
```

Remember the base is 16, so all numbers are hexadecimal. Indeed Fib(9)=$22=34.

Notice how we do not need to use recursion!


# References

This are the books, articles, papers or videos I've read/watched and that have helped me understand FORTH and make progress in this project.

- [Moving Forth: Part 1](https://www.bradrodriguez.com/papers/moving1.htm): explains the Direct and Indirect Threading Code models
- [Threaded Interpretive Languages Their Design And Implementation](https://archive.org/details/R.G.LoeligerThreadedInterpretiveLanguagesTheirDesignAndImplementationByteBooks1981), R. G. Loeliger, Byte Books (1981)
- [Bitwise](https://github.com/pervognsen/bitwise): an educational project where we create the software/hardware stack for a computer from scratch
  - Days 35 to 39 are about Implementing Forth: [Day 35 - Part 1](https://www.youtube.com/watch?v=rlayTh3sjiw), [Day 36 - Part 2](https://www.youtube.com/watch?v=SPErnyotJrk), [Day 37 - Part 3](https://www.youtube.com/watch?v=TA8blMaNqxY), [Day 38 - Part 4.1](https://www.youtube.com/watch?v=asW2hkGnsyM&t=6977s), [Day 38 - Part 4.2](https://www.youtube.com/watch?v=ceTRcHsXRzQ), [Day 39 - Part 5.1](https://www.youtube.com/watch?v=4Uy1Mq8p72w), [Day 39 - Part 5.2](https://www.youtube.com/watch?v=O6t14AGPO50)
- [JonesForth](https://github.com/nornagon/jonesforth/): A tutorial-like implementation of FORTH for the x86 with a lot of inline documentation.
- [Systems Guide to figForth](http://forth.org/OffeteStore/1010_SystemsGuideToFigForth.pdf)

Some more articles, routines or listings that have also been useful:
- [fig-FORTH 6502 assembly source listing](https://ksquiggle.neocities.org/ff6502.htm)
- [forthkit/eforth.forth](https://github.com/tehologist/forthkit/blob/master/eforth.forth)
- [6502.org: Source: Division (32-bit)](http://www.6502.org/source/integers/ummodfix/ummodfix.htm)
- [6502.org: Source: Multiply & Divide](http://www.6502.org/source/integers/32muldiv.htm)