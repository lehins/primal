# Changelog for `primal`

## 0.1.1

* Addition of `offToCount`, `offForType`, `offForProxyTypeOf`, `countToOff`,
  `countForType` and `countForProxyTypeOf`
* Deprecation of `offAsProxy` and `countAsProxy`
* Fix a big in `readOffAddr#` and `writeOffAddr#` for tuples.
* Fix an overflow bug in functions that use `memcmp`: `memcmpAddr#`,
  `memcmpAddrByteArray#`, `memcmpByteArray#` and `memcmpByteArrayAddr#`

## 0.1.0

* Initial release
