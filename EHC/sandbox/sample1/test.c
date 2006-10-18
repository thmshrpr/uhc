/* Generated C code */
#include "rts.h"

/* Tag constants */
#define CInt 0
#define CFalse 1
#define CTrue 2
#define Fundefined 3
#define Fmain 4

/* Global table */
GrWord global_main;
GrWord global_False;
GrWord global_True;
GrWord global_undefined;

/* Function prototypes */
void initialize(void);
void fun_main(void);

/* Function definitions */
void initialize(void) {
    BP = 0;
    global_main = (GrWord)heapalloc(10);
    ((Pointer)global_main)[0] = Fmain;
    global_False = (GrWord)heapalloc(10);
    ((Pointer)global_False)[0] = CFalse;
    global_True = (GrWord)heapalloc(10);
    ((Pointer)global_True)[0] = CTrue;
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

