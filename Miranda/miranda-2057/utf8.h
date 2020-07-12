typedef unsigned long unicode;
/* must be big enough to store codes up to UMAX */

#define UMAX 0x10ffff /* last unicode value */
#define BMPMAX 0xffff
#define EURO 0x20ac

#include <stdio.h>
unicode fromUTF8(FILE *);
void outUTF8(unicode, FILE *);
