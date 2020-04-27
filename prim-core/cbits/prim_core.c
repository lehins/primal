#include <HsFFI.h>
#include <string.h>


HsInt prim_core_ptreq(HsWord8 *ptr1, HsWord8 *ptr2){
  return ptr1 == ptr2;
}

HsInt8 prim_core_memcmp(HsWord8 *ptr1, HsInt offset1, HsWord8 *ptr2, HsInt offset2, HsInt n){
  ptr1+= offset1;
  ptr2+= offset2;
  return memcmp(ptr1, ptr2, n);
}

void prim_core_memmove(HsWord8 *src, HsInt src_offset, HsWord8 *dst, HsInt dst_offset, HsInt n){
  memmove(dst + dst_offset, src + src_offset, n);
}


void prim_core_memset8(HsWord8 *ptr, HsInt offset, HsInt n, HsWord8 x){
  memset((void *)(ptr + offset), x, n);
}

void prim_core_memset16(HsWord16 *ptr, HsInt offset, HsInt n, HsWord16 x){
  HsWord8 x8 = (HsWord8) x;
  ptr+= offset;
  if((HsWord8) (x >> 8) == x8){
    memset((void *)ptr, x8, n * 2);
    return;
  }
  while(n > 0){
    *ptr++ = x;
    n--;
  }
}

void prim_core_memset32(HsWord32 *ptr, HsInt offset, HsInt n, HsWord32 x){
  HsWord8 x8 = (HsWord8) x;
  ptr+= offset;
  // Use memset if it is a repeating 8bit pattern
  if((HsWord16) (x >> 16) == (HsWord16) x && (HsWord8) (x >> 24) == x8){
    memset((void *)ptr, x8, n * 4);
    return;
  }
  while(n > 0){
    *ptr++ = x;
    n--;
  }
}

void prim_core_memset64(HsWord64 *ptr, HsInt offset, HsInt n, HsWord64 x){
  HsWord8 x8 = (HsWord8) x;
  HsWord32 x32 = (HsWord32) x;
  ptr+= offset;
  // Use memset if it is a repeating 8bit pattern
  if((HsWord32) (x   >> 32) == x32 &&
     (HsWord16) (x32 >> 16) == (HsWord16) x32 &&
      (HsWord8) (x32 >> 24) == x8
     ){
    memset((void *)ptr, x8, n * 8);
    return;
  }
  // Allow gcc to vectorize with SIMD:
  HsWord32 *ptr32 = (HsWord32 *)ptr;
  const HsWord32 *x32p = (const HsWord32 *)(void *)&x;
  while (n > 0) {
    ptr32[0] = x32p[0];
    ptr32[1] = x32p[1];
    ptr32+= 2;
    n--;
  }
}


