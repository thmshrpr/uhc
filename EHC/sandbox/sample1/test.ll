target endian = little
target pointersize = 32
target triple = "i686-pc-linux-gnu"
deplibs = [ "c" ]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; External dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; C functions
declare int %printf(sbyte*, ...)
declare void %exit(int)

; Defined in runtime
declare void %heap_overflow_error()

%Heap       = external global %thunk_type*
%Stack      = external global %thunk_type*
%ReturnArea = external global %thunk_type*
%HeapLimit  = external global %thunk_type*

%HP = external global %thunk_type*
%SP = external global %thunk_type*
%RP = external global %thunk_type*
%BP = external global %thunk_type*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Typedefs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%thunk_type = type uint

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%global_main = global %thunk_type* undef
%global_False = global %thunk_type* undef
%global_True = global %thunk_type* undef
%global_undefined = global %thunk_type* undef

implementation 

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
  


