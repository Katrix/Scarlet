# Scarlet
A work in progress Scala decompiler and reverse engineering tool/library. There isn't much interesting stuff happening here yet. Come back later and maybe I've gotten further with this.

## Usage
```bash
java -jar scarlet.jar <input-file> [language]
```

The language specifies how far to parse and process the classfiles. Most will likely not be useful. 

The more common ones that you will probably normally use are:
* `scala-2.12`: TODO
* `java`: TODO

The complete list is.
* `raw-classfile`: Reads the classfile without resolving any references in the constant pool or any other further processing.
* `classfile`: Reads the classfile, resolves some references, but nothing more.
* `raw-bytecode`: Parses the bytecode of the methods, but does not resolve references to the constant pool, or do any other processing.
* `bytecode`: Reads the bytecode of the methods, resolves references to the constant pool, and removes specialization.
* `bytecode-syntax`: TODO
* `sir`: Scarlet's first IR. A stackless IR, close to but not exactly the same as bytecode. Based on the IR from this [paper](http://people.irisa.fr/David.Pichardie/papers/aplas10.pdf)
* `sir-syntax`: Same as `sir`, but prints it in a more readable format, looking more like normal code.
* `sir-cfg`: TODO
* `sir-cfg-syntax`: TODO

## Stuff that still needs to be done
* [ ] Structuring
* [ ] Try-Catch stuff
* [ ] Lambdas
* [ ] Rewrite if to expressions where it makes sense
* [ ] Rewrite try-catch-(finally) to expressions where it makes sense
* [ ] Rewrite match expressions ending with `MatchError`
* [ ] Rewrite match expressions not ending with `MatchError`
* [ ] Recover scala symbol names
* [ ] Rewrite top level vals
* [ ] Rewrite constructor vals
* [ ] Rewrite top level lazy vals
* [ ] Rewrite method level lazy vals
* [ ] Inline most values
* [ ] Remove most unit values
* [ ] Recover normal java generics
* [ ] Recover Scala's extended existentials
* [ ] Recover dependant types
* [ ] Recover higher kinded types
* [ ] Separate traits and classes again

## Goals
The end goal for Scarlet is to be able to decompile Java and Scala code and return a high level representation of the bytecode. This includes recovering information not present in the bytecode like generics and more advanced types like existentials, dependant and higher kinded types.

It should be able to do this both with and without the information provided by extra attributes found in the classfile.

## Where does the name come from?
Nowhere really, but if you really want an explanation, let's go with SCAla Reverse Language Engineering Tool.

## References
https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-2.html
https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html
https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-6.html
https://www.ndss-symposium.org/ndss2015/ndss-2015-programme/no-more-gotos-decompilation-using-pattern-independent-control-flow-structuring-and-semantics/
https://www.backerstreet.com/decompiler/frameless_functions.php
https://www.researchgate.net/publication/2645337_Analyzing_Control_Flow_in_Java_Bytecode
http://people.irisa.fr/David.Pichardie/papers/aplas10.pdf
