#include "rts.h"

PRIM int primXXXInt(int x, int y, int z)
{   return x+y+z;
}

PRIM int primAddInt(int x, int y)
{   return x+y;
}

PRIM int primSubInt(int x, int y)
{   return x-y;
}

PRIM int primMulInt(int x, int y)
{   return x*y;
}

PRIM int primDivInt(int x, int y)
{   return x/y;
}

PRIM int primModInt(int x, int y)
{   return x%y;
}

/* In the following 3 functions, only the constructor of Bool is returned.

   The arity of the constructor should also be returned, but we can return only one value.
   So, the arity will obtain a random value.
   Luckily, the arity is never used anywhere.
   (So I wonder why it is stored at all!)
*/

PRIM int primGtInt(int x, int y)
{   if (x>y)
        return ((Pointer)global_True)[0];
    return ((Pointer)global_False)[0];
}

PRIM int primLtInt(int x, int y)
{   if (x<y)
        return ((Pointer)global_True)[0];
    return ((Pointer)global_False)[0];
}

PRIM int primEqInt(int x, int y)
{   if (x==y)
        return ((Pointer)global_True)[0];
    return ((Pointer)global_False)[0];
}

PRIM GrWord primUnsafeId(GrWord x)
{   return x ;
}

PRIM int primUndefined()
{
    printf("attempt tot evaluate undefined\n");
    exit(1);
    return 0;
}