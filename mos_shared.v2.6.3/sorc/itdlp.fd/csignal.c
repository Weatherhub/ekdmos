/* C subroutine csignal. This routine allows for an interface
 * from itdlp (Fortran) to the C routine signal() that can
 * capture the SIGINT (Ctrl+C) signal while itdlp is running.
 *
 * This routine is called from the main itdlp program in the
 * following manner:
 *
 * CALL CSIGNAL(2,warning_sigint)
 *
 * The first value, 2 is the integer value for the SIGINT
 * signal and the second value passed is the routine to called
 * when the SIGINT signal is given. The Fortran subroutine,
 * warning_sigint simply for perform the STOP statement so
 * the program will quit. */

#include <signal.h>

typedef void (*sighandler_t)(int);

void csignal_( int* signum, sighandler_t handler)
{
    signal(*signum, handler);
}
