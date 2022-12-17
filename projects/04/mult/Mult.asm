// Multiplies R0 and R1 and stores the result in R2.
// (R0, R1, R2 refer to RAM[0], RAM[1], and RAM[2], respectively.)
// This program only needs to handle arguments that satisfy
// R0 >= 0, R1 >= 0, and R0*R1 < 32768.

// let's loop R1 times, adding R0 each time to sum

    @i // variables start at addresss 16 and increment from there
    M=1 //  i=1
    @sum // sum = 0
    M=0
(LOOP)
    @i // set A=i=1
    D=M // data register = i = 1
    @1
    D=D-M
    @END
    D;JGT // i-R1 > 0
    @0
    D=M
    @sum
    M=D+M
    @i
    M=M+1
    @LOOP
    0;JMP
(END)
    @sum
    D=M
    @2
    M=D
//    @END // infinite loop?
//    0;JMP
