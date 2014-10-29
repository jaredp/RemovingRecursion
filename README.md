RemovingRecursion
=================

Some Haskell code from my research on transforming recursion to tail recursion in functional algorithms

I think this is the best version of the code I wrote in 11th-12th grade for my work on [Compiling Away Recursion for Hardware](http://jaredp.github.io/assets/2013-pepm-recursion.pdf):

To provide a superior way of coding, compiling, and optimizing parallel algorithms, we are developing techniques for synthesizing hardware from functional specifications. Recursion, fundamental to functional languages, does not translate naturally to hardware, but tail recursion is iteration and easily implemented as a finite-state machine. In this paper, we show how to translate general recursion into tail recursion with an explicit stack that can be implemented in hardware. We give examples, describe the algorithm, and argue for its correctness. We also present experimental results that demonstrate the method is effective.

This requires ghc, but it also has a lot of other dependencies I don't remember.  I haven't looked at this code in a few years; if you want to use it, good luck.
