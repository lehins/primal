/* -----------------------------------------------------------------------------
 * $Id: longlong.c,v 1.4 2002/12/13 14:23:42 simonmar Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Primitive operations over (64-bit) long longs
 * (only used on 64-bit platforms for compatibility)
 *
 * ---------------------------------------------------------------------------*/


/*
Miscellaneous primitive operations on HsInt64 and HsWord64s.
N.B. These are not primops!

Instead of going the normal (boring) route of making the list
of primitive operations even longer to cope with operations
over 64-bit entities, we implement them instead 'out-of-line'.

The primitive ops get their own routine (in C) that implements
the operation, requiring the caller to _ccall_ out. This has
performance implications of course, but we currently don't
expect intensive use of either Int64 or Word64 types.

The exceptions to the rule are primops that cast to and from
64-bit entities (these are defined in PrimOps.h)
*/

#include "Rts.h"

#if WORD_SIZE_IN_BITS >= 64

/* Relational operators */

HsInt primal_gtWord64 (HsWord64 a, HsWord64 b) {return a >  b;}
HsInt primal_geWord64 (HsWord64 a, HsWord64 b) {return a >= b;}
HsInt primal_eqWord64 (HsWord64 a, HsWord64 b) {return a == b;}
HsInt primal_neWord64 (HsWord64 a, HsWord64 b) {return a != b;}
HsInt primal_ltWord64 (HsWord64 a, HsWord64 b) {return a <  b;}
HsInt primal_leWord64 (HsWord64 a, HsWord64 b) {return a <= b;}

HsInt primal_gtInt64 (HsInt64 a, HsInt64 b) {return a >  b;}
HsInt primal_geInt64 (HsInt64 a, HsInt64 b) {return a >= b;}
HsInt primal_eqInt64 (HsInt64 a, HsInt64 b) {return a == b;}
HsInt primal_neInt64 (HsInt64 a, HsInt64 b) {return a != b;}
HsInt primal_ltInt64 (HsInt64 a, HsInt64 b) {return a <  b;}
HsInt primal_leInt64 (HsInt64 a, HsInt64 b) {return a <= b;}

/* Arithmetic operators */

HsWord64 primal_remWord64  (HsWord64 a, HsWord64 b) {return a % b;}
HsWord64 primal_quotWord64 (HsWord64 a, HsWord64 b) {return a / b;}

HsInt64 primal_remInt64    (HsInt64 a, HsInt64 b)   {return a % b;}
HsInt64 primal_quotInt64   (HsInt64 a, HsInt64 b)   {return a / b;}
HsInt64 primal_negateInt64 (HsInt64 a)              {return -a;}
HsInt64 primal_plusInt64   (HsInt64 a, HsInt64 b)   {return a + b;}
HsInt64 primal_minusInt64  (HsInt64 a, HsInt64 b)   {return a - b;}
HsInt64 primal_timesInt64  (HsInt64 a, HsInt64 b)   {return a * b;}

/* Logical operators: */

HsWord64 primal_and64      (HsWord64 a, HsWord64 b) {return a & b;}
HsWord64 primal_or64       (HsWord64 a, HsWord64 b) {return a | b;}
HsWord64 primal_xor64      (HsWord64 a, HsWord64 b) {return a ^ b;}
HsWord64 primal_not64      (HsWord64 a)             {return ~a;}

HsWord64 primal_uncheckedShiftL64   (HsWord64 a, HsInt b)    {return a << b;}
HsWord64 primal_uncheckedShiftRL64  (HsWord64 a, HsInt b)    {return a >> b;}
/* Right shifting of signed quantities is not portable in C, so
   the behaviour you'll get from using these primops depends
   on the whatever your C compiler is doing. ToDo: fix. -- sof 8/98
*/
HsInt64  primal_uncheckedIShiftL64  (HsInt64 a,  HsInt b)    {return a << b;}
HsInt64  primal_uncheckedIShiftRA64 (HsInt64 a,  HsInt b)    {return a >> b;}
HsInt64  primal_uncheckedIShiftRL64 (HsInt64 a,  HsInt b)
                                    {return (HsInt64) ((HsWord64) a >> b);}

/* Casting between longs and longer longs.
*/

HsInt64  primal_intToInt64    (HsInt    i) {return (HsInt64)  i;}
HsInt    primal_int64ToInt    (HsInt64  i) {return (HsInt)    i;}
HsWord64 primal_int64ToWord64 (HsInt64  i) {return (HsWord64) i;}
HsWord64 primal_wordToWord64  (HsWord   w) {return (HsWord64) w;}
HsWord   primal_word64ToWord  (HsWord64 w) {return (HsWord)   w;}
HsInt64  primal_word64ToInt64 (HsWord64 w) {return (HsInt64)  w;}

#endif /* SUPPORT_LONG_LONGS */
