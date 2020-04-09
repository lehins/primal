* CInt is 32bits, should be CPtrdiff or at least an Int

foreign import ccall unsafe "primitive-memops.h hsprimitive_memmove"
  memmove_mba :: MutableByteArray# s -> CInt
              -> MutableByteArray# s -> CInt
              -> CSize -> IO ()

* This has a chance of ba1 be the same as ba2, but because GC moved it between the two
  coerces, it will not report as the same.

reallyUnsafePtrEquality# (unsafeCoerce# ba1 :: ()) (unsafeCoerce# ba2 :: ())
