# Changelog for `primal`

## 1.0.0

**Complete overhaul**

## 0.3.0

* Addition of `eval`, `evalM`, `deepeval` and `deepevalM`
* Addittion of `whenM` and `unlessM`
* Whole bunch of concurrency and exception functionality
* Addition of `Ref` adnd `MVar`
* Addition of basic array functionality:
  * Boxed array `BArray` and `BMArray`
  * Small boxed array `SBArray` and `SBMArray`
  * Unboxed array `UArray` and `UMArray`
* Move `Size` into `Data.Prim.Array` module
* Fix byte offset reading/writing compat functions for `Float`, `Double`, `Int16` and
  `Int32` for pre ghc-8.6
* Fix alignemnt for `()`, `Complex`, `Ratio` and `Fingerprint`
* Addition of internal to base function: `(#.)`

## 0.2.0

* Addition of `offToCount`, `offForType`, `countToOff` and `countForType`
* Renamed `offAsProxy` -> `offForProxyTypeOf` and `countAsProxy` -> `countForProxyTypeOf`
* Rename `fromCount` -> `unCountBytes`, `fromCount#` -> `unCountBytes#`, `fromOff#` ->
  `unOffBytes#`. Addition of `unOffBytes`
* Fix a big in `readOffAddr#` and `writeOffAddr#` for tuples
* Fix a big in `writeOffAddr#` for `Either`
* Fix an overflow bug in functions that use `memcmp`: `memcmpAddr#`,
  `memcmpAddrByteArray#`, `memcmpByteArray#` and `memcmpByteArrayAddr#`

## 0.1.0

* Initial release
