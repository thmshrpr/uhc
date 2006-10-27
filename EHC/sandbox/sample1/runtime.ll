target endian = little
target pointersize = 32
target triple = "i686-pc-linux-gnu"
deplibs = [ "c" ]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; External dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
declare int %printf(sbyte*, ...)
declare void %exit(int)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%__heapErrorMsg = internal constant [15 x sbyte] c"heap overflow\0A\00"
%__resultMsg    = internal constant [24 x sbyte] c"result tag=%d value=%d\0A\00"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

void %heap_overflow_error() {
  tail call int (sbyte*, ...)* %printf( sbyte* getelementptr ([15 x sbyte]* %__heapErrorMsg, int 0, int 0) )
  tail call void %exit( int 1 )
  unreachable
}
