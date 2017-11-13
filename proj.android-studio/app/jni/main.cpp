#include <stdlib.h>
#include "Main_stub.h"

int main(int argc, char **argv)
{
    // we need to unset language related environment
    // as NDK doesn't support unicode properly by default...
    putenv("LANG=");
    putenv("LC_ALL=");
    hs_init(&argc, &argv);
    runApp();
    hs_exit();
    return 0;
}