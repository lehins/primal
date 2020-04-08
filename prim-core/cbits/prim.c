#include "Rts.h"
#include <HsFFI.h>

/**
 * Rewrite of some Cmm in C. It is not in Cmm because `bdescr_flags` is not available
 * until this commit:
 * https://gitlab.haskell.org/ghc/ghc/commit/310371ff2d5b73cdcb2439b67170ca5e613541c0
 *
 * Cmm version can be found here:
 * https://gitlab.haskell.org/ghc/ghc/blob/4e8a71c1138b587dfbab8a1823b3f7fa6f0166bd/rts/PrimOps.cmm#L157-174
 *
 * Its types in Haskell are:
 * - `ByteArray# s -> Int#`
 * - `MutableByteArray# s -> Int#`
 *
 */
long prim_is_byte_array_pinned(StgPtr ba)
{
  bdescr *bd = Bdescr(ba);
  // All of BF_PINNED, BF_LARGE and BF_COMPACT are considered immovable. Although
  // BF_COMPACT is only available in ghc-8.2, so we don't care about it.
  return ((bd->flags & (BF_PINNED | BF_LARGE)) != 0);
}


void prim_memset8(HsWord8 *ptr, HsInt offset, HsInt n, HsWord8 x){
  ptr+= offset;
  while(n > 0){
    *ptr++ = x;
    n--;
  }
}

void prim_memset16(HsWord16 *ptr, HsInt offset, HsInt n, HsWord16 x){
  ptr+= offset;
  while(n > 0){
    *ptr++ = x;
    n--;
  }
}

void prim_memset32(HsWord32 *ptr, HsInt offset, HsInt n, HsWord32 x){
  ptr+= offset;
  while(n > 0){
    *ptr++ = x;
    n--;
  }
}

void prim_memset64(HsWord64 *ptr, HsInt offset, HsInt n, HsWord64 x){
  HsWord32 *ptr32 = (HsWord32 *)ptr;
  const HsWord32 *x32 = (const HsWord32 *)(void *)&x;
  while (n>0) {
    ptr32[0] = x32[0];
    ptr32[1] = x32[1];
    ptr32+= 2;
    n--;
  }
}

void prim_memsetFloat(HsFloat *ptr, HsInt offset, HsInt n, HsFloat x){
  ptr+= offset;
  while(n > 0){
    *ptr++ = x;
    n--;
  }
}

void prim_memsetDouble(HsDouble *ptr, HsInt offset, HsInt n, HsDouble x){
  HsFloat *ptr32 = (HsFloat *)ptr;
  const HsFloat *x32 = (const HsFloat *)(void *)&x;
  while (n>0) {
    ptr32[0] = x32[0];
    ptr32[1] = x32[1];
    ptr32+= 2;
    n--;
  }
}
