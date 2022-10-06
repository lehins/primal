# Changelog for primal-memory

# 1.0.0

* Addition of `FAddr` and `FMAddr`
* Rename `PArray` -> `PUArray` and `PMArray` -> `PUMArray`

## 0.3.0

* Rename many functions that perform mutation. Add `Mut` infix to operations that deal
  with mutable source memory regions.
* Rename `resizeMem` -> `reallocMutMem`
* Export `defaultReallocMutMem`
* Rename `PrimArray` -> `PArray` and `MPrimArray` -> `PMArray`

## 0.2.0

* Rename `ByteArray` -> `PrimArray`
* Improve `fromList` conversion
* Get rid of `allocByteCountMem` in favor of more general `allocMem`

## 0.1.0

* Initial release
