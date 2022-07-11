#include <sched.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/select.h>

int main(__attribute__((unused)) int argc, __attribute__((unused)) char *argv[]) {
	fd_set rfds; FD_ZERO(&rfds); FD_SET(0, &rfds);
	struct timeval tv; tv.tv_sec = 0; tv.tv_usec = 0;

	sched_yield();
	int retval = select(1, &rfds, NULL, NULL, &tv);
	if (retval == -1) {
		perror("select");
		return EXIT_FAILURE;
	} else if (retval) {
		return EXIT_SUCCESS;
	} else {
		return EXIT_FAILURE;
	}
}
