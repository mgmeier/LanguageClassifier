LanguageClassifier
==================

Language Classification based on [Bigram Model Similarity](http://en.wikipedia.org/wiki/Bigram).

This program lets you train different language models from
text data and save them as binary '.lmod' files. You can
then evaluate the similarity of further text or file input
to these models, and let the program guess the input language.

A simple read-eval-print loop (REPL) is included to toy around with.

This program just shows how it's done in Haskell and is published for its (hopefully) educational value.



Usage
=====

```git clone``` the repo, ```cd``` into it and type ```cabal run```


The REPL will start; have a look at your options:

```
(h for help) > h
e <file>  - load a file, evaluate differences to loaded models and make a guess
i <text>  - for some input, evaluate differences to loaded models and make a guess
t <file>  - load a file, train a model from its data
r         - rebuild models from <xy.txt> files, xy being a 2-letter language code
s         - show models loaded in memory
q         - quit
```


Now you can profit easily, see:

```
(h for help) > i kas sa oled sakslane?
[...]
I'm quite sure this is: et -- Eesti keel

(h for help) > i oletko sinä saksalainen?
[...]
I'm quite sure this is: fi -- Suomi
```