# Changelog for `primal`

## 0.2.0

* Addition of `offToCount`, `offForType`, `countToOff` and `countForType`
* Renamed `offAsProxy` -> `offForProxyTypeOf` and `countAsProxy` -> `countForProxyTypeOf`
* Rename `fromCount` -> `unCountBytes`, `fromCount#` -> `unCountBytes#`, `fromOff#` ->
  `unOffBytes#`. Addition of `unOffBytes`
* Fix a big in `readOffAddr#` and `writeOffAddr#` for tuples.
* Fix an overflow bug in functions that use `memcmp`: `memcmpAddr#`,
  `memcmpAddrByteArray#`, `memcmpByteArray#` and `memcmpByteArrayAddr#`

## 0.1.0

* Initial release
