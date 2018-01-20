#include <windows.h>

static nanotime_t get_nanotime(void) {
    LARGE_INTEGER time_var, frequency;
    QueryPerformanceCounter(&time_var);
    QueryPerformanceFrequency(&frequency);

    /* Convert to nanoseconds */
    return (nanotime_t)(1.0e9 * time_var.QuadPart / frequency.QuadPart);
}
