%%[8
#include "../../rts.h"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void mm_ssmutator_C_Init
		( MM_Mutator* mutator
		, MM_Malloc* memmgt
		, MM_Allocator* allocator
		, MM_Allocator* resAllocator
		, MM_Trace* trace
		, MM_Module* module
%%[[90
		, MM_WeakPtr* weakPtrAdm
		, MM_DEQue* weakPtrFinalizeQue
%%]]
		) 
{
	mutator->allocator = allocator ;
	mutator->residentAllocator = resAllocator ;
	mutator->trace = trace ;
	mutator->module = module ;
	mutator->malloc = memmgt ;
%%[[90
	mutator->weakPtrAdm = weakPtrAdm ;
	mutator->weakPtrFinalizeQue = weakPtrFinalizeQue ;
%%]]
    
}

Bool mm_ssmutator_C_IsMaintainedByGC( MM_Mutator* mutator, Word obj ) 
{
    return 1;
}

%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% special purpose allocation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[90
Ptr mm_ssmutator_C_Alloc_WeakPtr( MM_Mutator* mutator ) {
	return NULL;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% running a finalizer, an IO ()
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
void mm_ssmutator_C_RunFinalizer( MM_Mutator* mutator, Word finalizer ) {
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% MM_Mutator interface object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
MM_Mutator mm_ssmutator_C =
	{ NULL
	, NULL
	, NULL
	, NULL
	, NULL
	, NULL
%%[[90
	, NULL
	, NULL
%%]]
	, &mm_ssmutator_C_Init
	, &mm_ssmutator_C_IsMaintainedByGC
	// , &
%%[[90
	, &mm_ssmutator_C_Alloc_WeakPtr
%%]]
%%[[99
	, &mm_ssmutator_C_RunFinalizer
%%]]
	} ;
%%]

