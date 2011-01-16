#include <sys/time.h>

/* short an sweet! */
nanotime_t get_nanotime(void) {
    return gethrtime();
}    
