#include <cstdio>
#include <stdlib.h>
#include <string.h>
#include <cmath>

extern "C"

long djb2_hash(char *str)
{
    long hash = 5381;
    int c;

    while ((c = *str++))
        hash = ((hash << 5) + hash) + c; /* hash * 33 + c */

    return hash;
}

void* allocate(long size)
{
    return malloc(size);
}


