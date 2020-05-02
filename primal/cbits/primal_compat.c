#if __GLASGOW_HASKELL__ < 806

#include "Rts.h"
#include <HsFFI.h>
#include <string.h>

HsWord16 primal_memread16(const HsWord8 *ptr, HsInt offset){
  return *((HsWord16 *)(ptr + offset));
}
void primal_memwrite16(HsWord8 *ptr, HsInt offset, HsWord16 x){
  *((HsWord16 *)(ptr + offset)) = x;
}

HsWord32 primal_memread32(const HsWord8 *ptr, HsInt offset){
  return *((HsWord32 *)(ptr + offset));
}
void primal_memwrite32(HsWord8 *ptr, HsInt offset, HsWord32 x){
  *((HsWord32 *)(ptr + offset)) = x;
}

HsWord64 primal_memread64(const HsWord8 *ptr, HsInt offset){
  return *((HsWord64 *)(ptr + offset));
}
void primal_memwrite64(HsWord8 *ptr, HsInt offset, HsWord64 x){
  *((HsWord64 *)(ptr + offset)) = x;
}

#if __GLASGOW_HASKELL__ < 802
/**
 * Rewrite of some Cmm in C. It is not in Cmm because `bdescr_flags` is not available
 * until this commit:
 * https://gitlab.haskell.org/ghc/ghc/commit/310371ff2d5b73cdcb2439b67170ca5e613541c0
 *
 * Cmm version can be found here:
 * https://gitlab.haskell.org/ghc/ghc/blob/4e8a71c1138b587dfbab8a1823b3f7fa6f0166bd/rts/PrimOps.cmm#L157-174
 *
 * Its types in Haskell are:
 * - `ByteArray# -> Int#`
 * - `MutableByteArray# s -> Int#`
 *
 */
HsInt primal_is_byte_array_pinned(StgPtr ba){
  bdescr *bd = Bdescr(ba);
  // All of BF_PINNED, BF_LARGE and BF_COMPACT are considered immovable. Although
  // BF_COMPACT is only available in ghc-8.2, so we don't care about it.
  return ((bd->flags & (BF_PINNED | BF_LARGE)) != 0);
}
#endif /* __GLASGOW_HASKELL__ < 802 */


#endif /* __GLASGOW_HASKELL__ < 806 */
