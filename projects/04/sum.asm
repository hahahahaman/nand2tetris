// compute sum=1+...+100

    @i // A = location1
    M=1 // M[A]=1
    @sum // A = l2
    M=0 // M[A]=0
(LOOP)
    @i
    D=M // D=M[i]=1
    @100
    D=D-A // i-100
    @END
    D;JGT // i-100 > 0
    @i
    D=M
    @sum
    M=D+M // sum=sum+i
    @i
    M=M+1 // increment i
    @LOOP // goto loop
    0;JMP
(END)
    @END
    0;JMP
