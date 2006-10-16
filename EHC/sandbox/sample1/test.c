/* Generated C code */
#include "rts.h"

/* Tag constants */
#define CInt 0
#define Fundefined 1
#define Fmain 2

/* Global table */
GrWord global_main;
GrWord global_undefined;

/* Function prototypes */
void initialize(void);
void fun_main(void);

/* Function definitions */
void initialize(void) {
    BP = 0;
    global_main = (GrWord)heapalloc(10);
    ((Pointer)global_main)[0] = Fmain;
    global_undefined = (GrWord)heapalloc(10);
    ((Pointer)global_undefined)[0] = Fundefined;
}

void fun_main(void) {
    /* Enter */
    *SP = ((GrWord)BP);
    BP = SP;
    SP += 1 ;
    /* Unit */
    RP[0] = CInt;
    RP[1] = 1;
    /* Leave */
    SP = BP;
    BP = ((Pointer)*SP);
}

