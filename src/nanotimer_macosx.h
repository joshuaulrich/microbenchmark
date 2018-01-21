#include <mach/mach_time.h>

/* QA1398 [1] warns that this calculation may overflow uint64:
 * mach_absolute_time() * mach_timebase_info.numer / mach_timebase_info.denom
 *
 * The StackOverflow question [2] suggests that it's mainly a problem on
 * Power-PC Macs, since the numerator and denominator are both 1 on Intel
 * Macs.
 *
 * [1] https://developer.apple.com/library/content/qa/qa1398/_index.html
 * [2] https://stackoverflow.com/questions/23378063
 */
static nanotime_t get_nanotime(void) {
    static uint64_t ratio = 0;
    if (ratio == 0) {
        mach_timebase_info_data_t info;
        mach_timebase_info(&info);
        if ((info.numer % info.denom) == 0) {
            ratio = info.numer / info.denom;
        } else {
            warning("less accurate nanosecond times to avoid potential integer overflows");
            ratio = (uint64_t)((double)info.numer / info.denom);
        }
    }

    uint64_t time = mach_absolute_time();

    /* Convert to nanoseconds */
    return (nanotime_t)(time * ratio);
}
