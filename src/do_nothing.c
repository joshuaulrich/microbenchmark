#include "do_nothing.h"

/* Do nothing but do it well.
 * 
 * The purpose of this function is to have a callsite outside of the 
 * nanotimer.c compilation unit which forces the compiler to emit a 
 * call instruction instead of optimizing out the meaningless call. We 
 * want the compiler to include the call to include the overhead in our 
 * overhead estimation.
 */
int do_nothing(void) {
  return 42;
}
