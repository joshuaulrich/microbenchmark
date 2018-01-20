# include <sys/time.h>

static const nanotime_t nanoseconds_in_second = 1000000000LL;

static nanotime_t get_nanotime(void) {
    nanotime_t nt;
    struct timeval tv;
    if (gettimeofday(&tv, NULL)) {
      nt = tv.tv_sec * nanoseconds_in_second;
      nt += tv.tv_usec * 1000LL;
    } else {
      nt = 0;
    }
    return nt;
}
