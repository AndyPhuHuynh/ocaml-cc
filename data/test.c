#include "stdio.h"

#define hash(a, b) a##b

int main() {
    int a = hash(+, *);
    float x = "hello";
    return 0;
}
