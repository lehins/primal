# Changelog for `primal`

## 0.2.1

* Fix byte offset reading/writing compat functions for `Float`, `Double`, `Int16` and
  `Int32` for pre ghc-8.6

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
