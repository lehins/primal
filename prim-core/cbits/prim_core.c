#include "Rts.h"
#include <HsFFI.h>

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
long prim_core_is_byte_array_pinned(StgPtr ba){
  bdescr *bd = Bdescr(ba);
  // All of BF_PINNED, BF_LARGE and BF_COMPACT are considered immovable. Although
  // BF_COMPACT is only available in ghc-8.2, so we don't care about it.
  return ((bd->flags & (BF_PINNED | BF_LARGE)) != 0);
}
#endif

void prim_core_memmove(HsWord8 *src, HsInt src_offset, HSWord8 *dst, HsInt dst_offset, HsInt n){
  memmove(dst + dst_offset, src + src_offset, n);
}

/* HsInt8 prim_core_memcmp(HsWord8 *ptr1, HsWord8 *ptr2, HsInt n){ */
/*   //ptr1 == ptr */
/*   memcmp(ptr1, ptr2, n); */
/* } */

void prim_core_memset8(HsWord8 *ptr, HsInt offset, HsInt n, HsWord8 x){
  ptr+= offset;
  while(n > 0){
    *ptr++ = x;
    n--;
  }
}

void prim_core_memset16(HsWord16 *ptr, HsInt offset, HsInt n, HsWord16 x){
  ptr+= offset;
  while(n > 0){
    *ptr++ = x;
    n--;
  }
}

void prim_core_memset32(HsWord32 *ptr, HsInt offset, HsInt n, HsWord32 x){
  ptr+= offset;
  while(n > 0){
    *ptr++ = x;
    n--;
  }
}

void prim_core_memset64(HsWord64 *ptr, HsInt offset, HsInt n, HsWord64 x){
  HsWord32 *ptr32 = (HsWord32 *)ptr;
  const HsWord32 *x32 = (const HsWord32 *)(void *)&x;
  while (n>0) {
    ptr32[0] = x32[0];
    ptr32[1] = x32[1];
    ptr32+= 2;
    n--;
  }
}
