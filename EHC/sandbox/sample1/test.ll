target endian = little
target pointersize = 32
target triple = "i686-pc-linux-gnu"
deplibs = [ "c", "crtend" ]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; External dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
declare int %printf(sbyte*, ...)
declare void %exit(int)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%__heapErrorMsg = internal constant [15 x sbyte] c"heap overflow\0A\00"
%__resultMsg    = internal constant [24 x sbyte] c"result tag=%d value=%d\0A\00"	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Typedefs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%thunk_type = type uint

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%Heap = global %thunk_type* undef
%Stack = global %thunk_type* undef
%ReturnArea = global %thunk_type* undef

%HeapLimit = global %thunk_type* undef

%HP = global %thunk_type* undef
%SP = global %thunk_type* undef
%RP = global %thunk_type* undef
%BP = global %thunk_type* undef

%global_main = global %thunk_type* undef
%global_False = global %thunk_type* undef
%global_True = global %thunk_type* undef
%global_undefined = global %thunk_type* undef

implementation 

void %main( ) {

  ; Allocate the heap and save the pointer in %Heap and %HP
  %heap_ptr  = malloc %thunk_type, uint 100000
  store %thunk_type* %heap_ptr, %thunk_type** %Heap
  store %thunk_type* %heap_ptr, %thunk_type** %HP

  ; Get the last element of the allocated heap and save that in %HeapLimit
  %heap_limit_ptr = getelementptr %thunk_type* %heap_ptr, uint 99999
  store %thunk_type* %heap_limit_ptr, %thunk_type** %HeapLimit

  ; Allocate the stack
  %stack_ptr = malloc %thunk_type, uint 100000
  store %thunk_type* %stack_ptr, %thunk_type** %Stack
  store %thunk_type* %stack_ptr, %thunk_type** %SP  

  ; Allocate the returnpointer
  %ret_ptr   = malloc %thunk_type, uint 100    
  store %thunk_type* %ret_ptr, %thunk_type** %ReturnArea
  store %thunk_type* %ret_ptr, %thunk_type** %RP

  tail call void %initialize()
  tail call void %fun_main()

  ; Print RP[0] and RP[1]
  ;
  %rp_0_ptr = load %thunk_type** %RP
  %rp_1_ptr = getelementptr %thunk_type* %rp_0_ptr, uint 1
  %rp_0     = load %thunk_type* %rp_0_ptr
  %rp_1     = load %thunk_type* %rp_1_ptr
  tail call int (sbyte*, ...)* %printf( sbyte* getelementptr ([24 x sbyte]* %__resultMsg, int 0, int 0)
                                      , %thunk_type %rp_0, %thunk_type %rp_1 )
  ret void
}

void %initialize( ) {

  %BP_deref = load %thunk_type** %BP
  store %thunk_type 0, %thunk_type* %BP_deref

  %tmp.0 = tail call %thunk_type* %heapalloc( uint 10 )
  store %thunk_type 4, %thunk_type* %tmp.0
  store %thunk_type* %tmp.0, %thunk_type** %global_main

  %tmp.1 = tail call %thunk_type* %heapalloc( uint 10 )
  store %thunk_type 1, %thunk_type* %tmp.1
  store %thunk_type* %tmp.1, %thunk_type** %global_False

  %tmp.2 = tail call %thunk_type* %heapalloc( uint 10 )
  store %thunk_type 2, %thunk_type* %tmp.2
  store %thunk_type* %tmp.2, %thunk_type** %global_main

  %tmp.3 = tail call %thunk_type* %heapalloc( uint 10 )
  store %thunk_type 3, %thunk_type* %tmp.3
  store %thunk_type* %tmp.3, %thunk_type** %global_main

  ret void
}

void %fun_main( ) {

  ; Save the current pointer value of %BP in the %SP pointer
  ; for that we need to cast the %thunk_type* to a %thunk_type
  ; which only works if the pointersize is equal to the %thunk_type size
  ;
  %BP_deref = load %thunk_type** %BP   
  %SP_deref = load %thunk_type** %SP
  %cast_BP  = cast %thunk_type* %BP_deref to %thunk_type
  store %thunk_type %cast_BP, %thunk_type* %SP_deref

  ; Overwrite the %BP with the %SP value
  ;
  store %thunk_type* %SP_deref, %thunk_type** %BP

  ; Raise the %SP by one
  ;
  %raised_SP = getelementptr %thunk_type* %SP_deref, uint 1
  store %thunk_type* %raised_SP, %thunk_type** %SP

  ; Fill the ReturnArea
  ;
  %RP_deref_0 = load %thunk_type** %RP
  %RP_deref_1 = getelementptr %thunk_type* %RP_deref_0, uint 1
  store %thunk_type 0, %thunk_type* %RP_deref_0
  store %thunk_type 10, %thunk_type* %RP_deref_1

  ; Return the %SP to the old value (stored in the %BP)
  ;
  store %thunk_type* %SP_deref, %thunk_type** %SP
  
  ; Recast the %cast_BP back to a pointer and store it in the %BP
  ;
  %recast_BP = cast %thunk_type %cast_BP to %thunk_type*
  store %thunk_type* %recast_BP, %thunk_type** %BP
  ret void
}

%thunk_type* %heapalloc( uint %words ) {

  ; Offset of current pointer is 0 based
  %word_offset = sub uint %words, 1 

  ; The thunk can be allocated on this address
  %curr_ptr = load %thunk_type** %HP
  %new_ptr  = getelementptr %thunk_type* %curr_ptr, uint %word_offset
  %heap_limit = load %thunk_type** %HeapLimit

  ; Check if the new heap pointer is below the heap limit
  %heap_limit_cond = setge %thunk_type* %new_ptr, %heap_limit
  br bool %heap_limit_cond, label %heap_overflow, label %no_heap_overflow

  heap_overflow:
  tail call void %heap_overflow_error()
  ret %thunk_type* %curr_ptr ; Declared unreachable by the heap_overflow_error

  no_heap_overflow:
  ret %thunk_type* %curr_ptr
}

void %heap_overflow_error() {
  tail call int (sbyte*, ...)* %printf( sbyte* getelementptr ([15 x sbyte]* %__heapErrorMsg, int 0, int 0) )
  tail call void %exit( int 1 )
  unreachable
}
  


