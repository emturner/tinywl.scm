#include <time.h>
#include <libguile.h>

SCM
clock_gettime_monotomic_wrapper ()
{
    struct timespec *now = malloc(sizeof(*now));

    clock_gettime (CLOCK_MONOTONIC, now);

    return scm_from_pointer (now, NULL);
}

void
init_clock_time (void)
{
    scm_c_define_gsubr("clock-gettime-monotomic", 0, 0, 0, clock_gettime_monotomic_wrapper);
}
