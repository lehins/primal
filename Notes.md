
* This has a chance of ba1 be the same as ba2, but because GC moved it between the two
  coerces, it will not report as the same.

reallyUnsafePtrEquality# (unsafeCoerce# ba1 :: ()) (unsafeCoerce# ba2 :: ())


`primitive` limitations:

* `Prim` class: inability to index/read/write with Byte offset
* Lack of backwards compatibility (eg, isPinnedByteArray only works on ghc-8.2 and up)
* Lack of unified interface



=== Plan


* Finish raw implementation of:
  * primal
  * primal-memory (ByteArray, MByteArray, Addr, MAddr, Bytes, MBytes)
  * primal-mutable
  * primal-pvar



* Future:
  * primal (Int8, Int16 ...)
  * primal-simd
  * primal-memory (ForeignAddr, ForeignMAddr)
  * primal-numeric
