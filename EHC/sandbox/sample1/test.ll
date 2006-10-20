target endian = little
target pointersize = 32
target triple = "i686-pc-linux-gnu"

%thunk_type = type < 8 x uint >

;%MAX_HEAP_THUNKS = global uint 100
;%THUNK_SIZE      = global uint 10
;%HEAP_LIMIT      = global uint undef

;%HP = global uint undef

implementation 



void %main( ) {

  %thunk_ptr = malloc %thunk_type, uint 1  

}

;uint %heapalloc( uint %words ) {
;
;  %curr = load uint* %HP
;  %new  = add uint %words, %curr
;  store uint %new, uint* %HP
;  ret uint %new
;}


;uint %main( ) {
;
;  tail call uint %heapalloc( uint 4 )
;  %curr = tail call uint %heapalloc( uint 5 )
;  ret uint %curr
;}
  


