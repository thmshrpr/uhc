target endian = little
target pointersize = 32
target triple = "i686-pc-linux-gnu"

%X = global int 0

implementation 

int %main() {

  %var0 = load int* %X
  switch int %var0, label %default1 [ int 0, label %switch2 ]
                                      ;int 1, label %switch3 ]
switch2:
  ret int 5
switch3:
  ret int 6
default1: 
  ret int %var0
}
