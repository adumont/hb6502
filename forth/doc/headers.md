# Headers in AlexForth

In this article, we look at the header stucture of words in AlexForth.

Let's define a couple of words. Notice we use `HERE` before defining the words, and `HERE` again when we finished. At the end on the stack we have the start and end address of the exact region of the dictionary containing the two words. For each word, it will contain the header and the code of the word.

```
ok HERE .S
02FA

ok : WORD1   1 1 + . ;
ok : DOUBLE  DUP +   ;

ok HERE .S
02FA 0321
```

Now let's `DUMP` the memory containing the two words:

```
ok DUMP
02FA E8 02 05 57  4F 52 44 31  4C 77 82 0A  83 0A 83 21
     8E ED 96 94  82 FA 02 06  44 4F 55 42  4C 45 4C 77
     82 15 8F 21  8E 94 82
ok
```

Let's use `SEE` to see the definition of the words' definitions in HEX, which will help us make sense of most of the memory dump.

```
ok SEE WORD1
0303 8277 COLON
0305 830A 1
0307 830A 1
0309 8E21 +
030B 96ED .
030D 8294 EXIT

ok SEE DOUBLE
0319 8277 COLON
031B 8F15 DUP
031D 8E21 +
031F 8294 EXIT
ok
```

Now, let's reformat the memory dump, and add comments on the right. First we have the header of the word `WORD1`, followed by its code definition:

The header is made of 2 fields:
- The **link field** (two bytes), where we keep the address of the previous word in the dictionary. (Remember the dictionary is a *linked list*!)
- The **name field**, where the name of the word is stored as a counted string (1 byte for the length, followed by the name). Note that the length byte is also used to encode flags (only the 3 msb)

The address of these fields in the header is usually called **LFA** (link field address), and **NFA** (name field address) respectively.

Right after the header comes the code for the word. The corresponding address is usually called **CFA** (Code field address).

```
02FA E8 02                  LFA  02E8 = Addr of previous word in dictionary
     05                     NFA  len=5
     57 4F 52 44 31              W, O, R, D, 1

     4C 77 82               CFA  JMP 8277 COLON
     0A 83                       830A 1
     0A 83                       830A 1
     21 8E                       8E21 +
     ED 96                       96ED .
     94 82                       8294 EXIT
```

Right after the definition of `WORD1`, we find the header of the word `DOUBLE`, also followed by its code definition.

```
     FA 02                  LFA  02FA = Addr of previous word (WORD1 above)
     06                     NFA  len=6
     44 4F 55 42 4C 45           D, O, U, B, L, E

     4C 77 82               CFA  JMP 8277 COLON
     15 8F                       8F15 DUP
     21 8E                       8E21 +
     94 82                       8294 EXIT
ok
```

What about the PFA?

As far as I know, **PFA** stands for Parameter Field Address. In fact it's more a *payload* than a parameter (in the sense I made of the field). For normal words, I don't think it makes much sense, but for words like variables, it does. The PFA is where the value of the variable will go. In the particular case of a variable, the PFA will be after the call to do_VAR.

```
ok HERE
ok VARIABLE VAR1
ok 0 VAR1 !
ok HERE DUMP
ok
ok 02D3 HERE DUMP
02D3 B5 02 04 56  41 52 31 4C  BD 83 00 00
ok
```

We can see the PFA in the following reformated and commented dump:

```
02D3     LFA    B5 02           Link to previous word
         NFA    04              len=4
                56 41 52 31     V, A, R, 1
         CFA    4C BD 83        JMP do_VAR
         PFA    00 00           0000 (value stored in the variable, at PFA)
ok
```

Now you should better understand how words are layed out in memory. Be sure to also read [the anatomy of compiled words](anatomy.md) to get a better understanding of how different types of words are compiles, with structures like conditionals, loops, and also `CREATE`'d words!