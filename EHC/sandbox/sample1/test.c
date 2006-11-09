/* Generated C code */
#include "rts.h"

/* Tag constants */
#define CInt 0
#define P/1id 1
#define Fundefined 2
#define Fmain 3

/* Global table */
GrWord global_main;
GrWord global_undefined;

/* Function prototypes */
void initialize(void);
void fun_id(void);
void fun_f(void);
void fun_main(void);

/* Function definitions */
void initialize(void) {
    BP = 0;
    global_main = heapalloc(10);
    ((Pointer)global_main)[0] = Fmain;
    global_undefined = heapalloc(10);
    ((Pointer)global_undefined)[0] = Fundefined;
}

void fun_id(void) {
    /* Local names */
    GrWord x__1;
    /* Parameters */
    x__1 = SP[-1];
    /* Enter */
    *SP = ((GrWord)BP);
    BP = SP;
    SP += 1 ;
    /* Unit */
    RP[0] = CInt;
    RP[1] = 4;
    /* Leave */
    SP = BP;
    BP = ((Pointer)*SP);
}

void fun_f(void) {
    /* Local names */
    GrWord h__3;
    /* Parameters */
    h__3 = SP[-1];
    /* Enter */
    *SP = ((GrWord)BP);
    BP = SP;
    SP += 1 ;
    /* Unit */
    RP[0] = P/1id;
    /* Leave */
    SP = BP;
    BP = ((Pointer)*SP);
}

void fun_main(void) {
    
    /* Local names */
    GrWord x_15;
    /* Enter */
    *SP = ((GrWord)BP);
    BP = SP;
    SP += 2 ;
    /* Store */
    x_15 = heapalloc(10);
    ((Pointer)x_15)[0] = CInt;
    ((Pointer)x_15)[1] = 60;
    /* Call (Tail) */
    RP[1] = x_15;
    /* Leave */
    SP = BP;
    BP = ((Pointer)*SP);
    SP += 1 ;
    SP[-1] = RP[1];
    asm("leave") ;
    register void* _tail_call_dest = ((void*)fun_id);
    
    goto *_tail_call_dest;
}

