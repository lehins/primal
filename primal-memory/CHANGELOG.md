# Changelog for primal-memory

## 0.3.0

* Rename many functions that perform mutation. Add `Mut` infix to operations that deal
  with mutable source memory regions.
* Rename `resizeMem` -> `reallocMutMem`
* Export `defaultReallocMutMem`

## 0.2.0

* Rename `ByteArray` -> `PrimArray`
* Improve `fromList` conversion
* Get rid of `allocByteCountMem` in favor of more general `allocMem`

## 0.1.0

* Initial release
