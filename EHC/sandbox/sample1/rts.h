#include <stdio.h>
#include "config.h"

typedef unsigned int GrWord;
typedef GrWord* Pointer;

/* the empty PRIM define is used to mark exported functions from prim.c,
   used to automatically generate prim.h
*/
#define PRIM

#include "prim.h"

#if USE_BOEHM_GC
#include "gc.h"
#else

#define HEAPSIZE 100000

extern Pointer HP;
extern Pointer Heap;
extern Pointer HeapEndCAF, HeapLimit;

#endif

#define STACKSIZE 100000
#define RETURNSIZE 100

extern Pointer SP, RP, BP;
extern Pointer Stack, ReturnArea;

extern GrWord global_False;
extern GrWord global_True;
