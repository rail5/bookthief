#ifndef STAT
#define STAT
#include <sys/stat.h>
#endif

inline bool file_exists(const char *name) {
	struct stat buffer;
	return (stat (name, &buffer) == 0);
}
