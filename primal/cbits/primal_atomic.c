#include <HsFFI.h>

/* See GCC reference for all of the atomic opartions in this file
 * https://gcc.gnu.org/onlinedocs/gcc-9.3.0/gcc/_005f_005fsync-Builtins.html
 */

void primal_sync_synchronize (void) {
  return __sync_synchronize ();
}

// There are systems that do not support other values except 1 and 0. Moreover, in `atomic`
// package it is used only as a lock, so having 1 and 0 as only values is sufficient for now.
HsInt primal_sync_lock_test_set(HsInt mba[], HsInt i) {
  return __sync_lock_test_and_set(&mba[i], 1);
}

void primal_sync_lock_release(HsInt mba[], HsInt i) {
  return __sync_lock_release(&mba[i]);
}

HsWord8 primal_sync8_cas (HsWord8 mba[], HsInt i, HsWord8 old, HsWord8 new) {
	return __sync_val_compare_and_swap (&mba[i], old, new);
}
HsWord8 primal_sync8_fetch_add(HsWord8 mba[], HsInt i, HsWord8 a){
	return __sync_fetch_and_add (&mba[i], a);
}
HsWord8 primal_sync8_add_fetch(HsWord8 mba[], HsInt i, HsWord8 a){
	return __sync_add_and_fetch (&mba[i], a);
}
HsWord8 primal_sync8_fetch_sub(HsWord8 mba[], HsInt i, HsWord8 a){
	return __sync_fetch_and_sub (&mba[i], a);
}
HsWord8 primal_sync8_sub_fetch(HsWord8 mba[], HsInt i, HsWord8 a){
	return __sync_sub_and_fetch (&mba[i], a);
}
HsWord8 primal_sync8_fetch_and(HsWord8 mba[], HsInt i, HsWord8 a){
	return __sync_fetch_and_and (&mba[i], a);
}
HsWord8 primal_sync8_and_fetch(HsWord8 mba[], HsInt i, HsWord8 a){
	return __sync_and_and_fetch (&mba[i], a);
}
HsWord8 primal_sync8_fetch_nand(HsWord8 mba[], HsInt i, HsWord8 a){
	return __sync_fetch_and_nand (&mba[i], a);
}
HsWord8 primal_sync8_nand_fetch(HsWord8 mba[], HsInt i, HsWord8 a){
	return __sync_nand_and_fetch (&mba[i], a);
}
HsWord8 primal_sync8_fetch_or(HsWord8 mba[], HsInt i, HsWord8 a){
	return __sync_fetch_and_or (&mba[i], a);
}
HsWord8 primal_sync8_or_fetch(HsWord8 mba[], HsInt i, HsWord8 a){
	return __sync_or_and_fetch (&mba[i], a);
}
HsWord8 primal_sync8_fetch_xor(HsWord8 mba[], HsInt i, HsWord8 a){
	return __sync_fetch_and_xor (&mba[i], a);
}
HsWord8 primal_sync8_xor_fetch(HsWord8 mba[], HsInt i, HsWord8 a){
	return __sync_xor_and_fetch (&mba[i], a);
}



HsWord16 primal_sync16_cas (HsWord16 mba[], HsInt i, HsWord16 old, HsWord16 new) {
	return __sync_val_compare_and_swap (&mba[i], old, new);
}
HsWord16 primal_sync16_fetch_add(HsWord16 mba[], HsInt i, HsWord16 a){
	return __sync_fetch_and_add (&mba[i], a);
}
HsWord16 primal_sync16_add_fetch(HsWord16 mba[], HsInt i, HsWord16 a){
	return __sync_add_and_fetch (&mba[i], a);
}
HsWord16 primal_sync16_fetch_sub(HsWord16 mba[], HsInt i, HsWord16 a){
	return __sync_fetch_and_sub (&mba[i], a);
}
HsWord16 primal_sync16_sub_fetch(HsWord16 mba[], HsInt i, HsWord16 a){
	return __sync_sub_and_fetch (&mba[i], a);
}
HsWord16 primal_sync16_fetch_and(HsWord16 mba[], HsInt i, HsWord16 a){
	return __sync_fetch_and_and (&mba[i], a);
}
HsWord16 primal_sync16_and_fetch(HsWord16 mba[], HsInt i, HsWord16 a){
	return __sync_and_and_fetch (&mba[i], a);
}
HsWord16 primal_sync16_fetch_nand(HsWord16 mba[], HsInt i, HsWord16 a){
	return __sync_fetch_and_nand (&mba[i], a);
}
HsWord16 primal_sync16_nand_fetch(HsWord16 mba[], HsInt i, HsWord16 a){
	return __sync_nand_and_fetch (&mba[i], a);
}
HsWord16 primal_sync16_fetch_or(HsWord16 mba[], HsInt i, HsWord16 a){
	return __sync_fetch_and_or (&mba[i], a);
}
HsWord16 primal_sync16_or_fetch(HsWord16 mba[], HsInt i, HsWord16 a){
	return __sync_or_and_fetch (&mba[i], a);
}
HsWord16 primal_sync16_fetch_xor(HsWord16 mba[], HsInt i, HsWord16 a){
	return __sync_fetch_and_xor (&mba[i], a);
}
HsWord16 primal_sync16_xor_fetch(HsWord16 mba[], HsInt i, HsWord16 a){
	return __sync_xor_and_fetch (&mba[i], a);
}



HsWord32 primal_sync32_cas (HsWord32 mba[], HsInt i, HsWord32 old, HsWord32 new) {
	return __sync_val_compare_and_swap (&mba[i], old, new);
}
HsWord32 primal_sync32_fetch_add(HsWord32 mba[], HsInt i, HsWord32 a){
	return __sync_fetch_and_add (&mba[i], a);
}
HsWord32 primal_sync32_add_fetch(HsWord32 mba[], HsInt i, HsWord32 a){
	return __sync_add_and_fetch (&mba[i], a);
}
HsWord32 primal_sync32_fetch_sub(HsWord32 mba[], HsInt i, HsWord32 a){
	return __sync_fetch_and_sub (&mba[i], a);
}
HsWord32 primal_sync32_sub_fetch(HsWord32 mba[], HsInt i, HsWord32 a){
	return __sync_sub_and_fetch (&mba[i], a);
}
HsWord32 primal_sync32_fetch_and(HsWord32 mba[], HsInt i, HsWord32 a){
	return __sync_fetch_and_and (&mba[i], a);
}
HsWord32 primal_sync32_and_fetch(HsWord32 mba[], HsInt i, HsWord32 a){
	return __sync_and_and_fetch (&mba[i], a);
}
HsWord32 primal_sync32_fetch_nand(HsWord32 mba[], HsInt i, HsWord32 a){
	return __sync_fetch_and_nand (&mba[i], a);
}
HsWord32 primal_sync32_nand_fetch(HsWord32 mba[], HsInt i, HsWord32 a){
	return __sync_nand_and_fetch (&mba[i], a);
}
HsWord32 primal_sync32_fetch_or(HsWord32 mba[], HsInt i, HsWord32 a){
	return __sync_fetch_and_or (&mba[i], a);
}
HsWord32 primal_sync32_or_fetch(HsWord32 mba[], HsInt i, HsWord32 a){
	return __sync_or_and_fetch (&mba[i], a);
}
HsWord32 primal_sync32_fetch_xor(HsWord32 mba[], HsInt i, HsWord32 a){
	return __sync_fetch_and_xor (&mba[i], a);
}
HsWord32 primal_sync32_xor_fetch(HsWord32 mba[], HsInt i, HsWord32 a){
	return __sync_xor_and_fetch (&mba[i], a);
}


HsWord primal_sync_cas (HsWord mba[], HsInt i, HsWord old, HsWord new) {
	return __sync_val_compare_and_swap (&mba[i], old, new);
}
HsWord primal_sync_fetch_add(HsWord mba[], HsInt i, HsWord a){
	return __sync_fetch_and_add (&mba[i], a);
}
HsWord primal_sync_add_fetch(HsWord mba[], HsInt i, HsWord a){
	return __sync_add_and_fetch (&mba[i], a);
}
HsWord primal_sync_fetch_sub(HsWord mba[], HsInt i, HsWord a){
	return __sync_fetch_and_sub (&mba[i], a);
}
HsWord primal_sync_sub_fetch(HsWord mba[], HsInt i, HsWord a){
	return __sync_sub_and_fetch (&mba[i], a);
}
HsWord primal_sync_fetch_and(HsWord mba[], HsInt i, HsWord a){
	return __sync_fetch_and_and (&mba[i], a);
}
HsWord primal_sync_and_fetch(HsWord mba[], HsInt i, HsWord a){
	return __sync_and_and_fetch (&mba[i], a);
}
HsWord primal_sync_fetch_nand(HsWord mba[], HsInt i, HsWord a){
	return __sync_fetch_and_nand (&mba[i], a);
}
HsWord primal_sync_nand_fetch(HsWord mba[], HsInt i, HsWord a){
	return __sync_nand_and_fetch (&mba[i], a);
}
HsWord primal_sync_fetch_or(HsWord mba[], HsInt i, HsWord a){
	return __sync_fetch_and_or (&mba[i], a);
}
HsWord primal_sync_or_fetch(HsWord mba[], HsInt i, HsWord a){
	return __sync_or_and_fetch (&mba[i], a);
}
HsWord primal_sync_fetch_xor(HsWord mba[], HsInt i, HsWord a){
	return __sync_fetch_and_xor (&mba[i], a);
}
HsWord primal_sync_xor_fetch(HsWord mba[], HsInt i, HsWord a){
	return __sync_xor_and_fetch (&mba[i], a);
}


