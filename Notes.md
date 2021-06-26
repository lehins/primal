
* This has a chance of ba1 be the same as ba2, but because GC moved it between the two
  coerces, it will not report as the same.

reallyUnsafePtrEquality# (unsafeCoerce# ba1 :: ()) (unsafeCoerce# ba2 :: ())


`primitive` limitations:

* `Prim` class: inability to index/read/write with Byte offset
* Lack of backwards compatibility (eg, isPinnedByteArray only works on ghc-8.2 and up)
* Lack of unified interface

`vector` limitations

* `Unbox` is not suiatable for sum types (i.e. `Maybe`, `Either`)
* `Unbox` works only on `Vector`, while `Prim` works for anything backed by `ByteArray`
* Singleton `PVar` backed by `Vector` is too wasteful (size and offset are useless)


=== Plan


* Finish raw implementation of:
  * primal
  * primal-memory (ByteArray, MByteArray, Addr, MAddr, Bytes, MBytes)
  * primal-container



* Future:
  * primal (Int8, Int16 ...)
  * primal-simd
  * primal-memory (ForeignAddr, ForeignMAddr)
  * primal-numeric
