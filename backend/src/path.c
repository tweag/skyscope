#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#define MAX_NODE_COUNT = 0x100000

int32_t c_findPath(
        int32_t origin,
        int32_t destination,
        const int32_t* stepMap,
        int32_t stepMapSize,
        int32_t* buffer,
        int32_t size
        ) {

    char buf[1024];
    snprintf(buf, sizeof(buf), "findPath(size = '%d')\n", size);
    write(2, buf, strlen(buf));
    return 0;
}

static int counter = 0;

int32_t c_indexPaths(
        int32_t destination,
        const int32_t* predMap,
        int32_t predMapSize,
        int32_t* stepMap,
        int32_t stepMapSize
        ) {

    ++counter;
    char buf[1024];
    //snprintf(buf, sizeof(buf), "\x1b[1;36mindexPath(destination = '%d')\x1b[0m\n", destination);
    //write(2, buf, strlen(buf));
    int len = rand() % 1024 + 8;
    for (int i = 0; i < len; ++i) {
        char a = 'A' + (rand() % 26);
        char b = 'A' + (rand() % 26);
        char c = 'A' + (rand() % 26);
        char d = 'A' + (rand() % 26);
        stepMap[i] = (a << 24) | (b << 16) | (c << 8) | (d << 0);
    }
    return len;
}
