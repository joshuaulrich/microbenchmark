#include <time.h>

nanotime_t get_nanotime(void) {
    struct timespec time_var;
    clock_gettime(CLOCK_REALTIME, &time_var);

    nanotime_t sec = time_var.tv_sec;
    nanotime_t nsec = time_var.tv_nsec;
    /* Convert to nanoseconds */
    return (sec * 1000000000) + nsec;
}
