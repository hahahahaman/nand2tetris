@256
D=A
@SP
M=D
@300
D=A
@LCL
M=D
@400
D=A
@ARG
M=D
@3000
D=A
@THIS
M=D
@3010
D=A
@THAT
M=D
@17
D=A
@SP
A=M
M=D
@SP
M=M+1
@17
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
M=M-1
@SP
A=M
D=M
@SP
M=M-1
A=M
D=M-D
@VMLABEL0
D;JEQ
@SP
A=M
M=0
@VMLABEL1
0;JMP
(VMLABEL0)
@SP
A=M
M=-1
(VMLABEL1)
@SP
M=M+1
@17
D=A
@SP
A=M
M=D
@SP
M=M+1
@16
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
M=M-1
@SP
A=M
D=M
@SP
M=M-1
A=M
D=M-D
@VMLABEL2
D;JEQ
@SP
A=M
M=0
@VMLABEL3
0;JMP
(VMLABEL2)
@SP
A=M
M=-1
(VMLABEL3)
@SP
M=M+1
@16
D=A
@SP
A=M
M=D
@SP
M=M+1
@17
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
M=M-1
@SP
A=M
D=M
@SP
M=M-1
A=M
D=M-D
@VMLABEL4
D;JEQ
@SP
A=M
M=0
@VMLABEL5
0;JMP
(VMLABEL4)
@SP
A=M
M=-1
(VMLABEL5)
@SP
M=M+1
@892
D=A
@SP
A=M
M=D
@SP
M=M+1
@891
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
M=M-1
@SP
A=M
D=M
@SP
M=M-1
A=M
D=M-D
@VMLABEL6
D;JLT
@SP
A=M
M=0
@VMLABEL7
0;JMP
(VMLABEL6)
@SP
A=M
M=-1
(VMLABEL7)
@SP
M=M+1
@891
D=A
@SP
A=M
M=D
@SP
M=M+1
@892
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
M=M-1
@SP
A=M
D=M
@SP
M=M-1
A=M
D=M-D
@VMLABEL8
D;JLT
@SP
A=M
M=0
@VMLABEL9
0;JMP
(VMLABEL8)
@SP
A=M
M=-1
(VMLABEL9)
@SP
M=M+1
@891
D=A
@SP
A=M
M=D
@SP
M=M+1
@891
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
M=M-1
@SP
A=M
D=M
@SP
M=M-1
A=M
D=M-D
@VMLABEL10
D;JLT
@SP
A=M
M=0
@VMLABEL11
0;JMP
(VMLABEL10)
@SP
A=M
M=-1
(VMLABEL11)
@SP
M=M+1
@32767
D=A
@SP
A=M
M=D
@SP
M=M+1
@32766
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
M=M-1
@SP
A=M
D=M
@SP
M=M-1
A=M
D=M-D
@VMLABEL12
D;JGT
@SP
A=M
M=0
@VMLABEL13
0;JMP
(VMLABEL12)
@SP
A=M
M=-1
(VMLABEL13)
@SP
M=M+1
@32766
D=A
@SP
A=M
M=D
@SP
M=M+1
@32767
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
M=M-1
@SP
A=M
D=M
@SP
M=M-1
A=M
D=M-D
@VMLABEL14
D;JGT
@SP
A=M
M=0
@VMLABEL15
0;JMP
(VMLABEL14)
@SP
A=M
M=-1
(VMLABEL15)
@SP
M=M+1
@32766
D=A
@SP
A=M
M=D
@SP
M=M+1
@32766
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
M=M-1
@SP
A=M
D=M
@SP
M=M-1
A=M
D=M-D
@VMLABEL16
D;JGT
@SP
A=M
M=0
@VMLABEL17
0;JMP
(VMLABEL16)
@SP
A=M
M=-1
(VMLABEL17)
@SP
M=M+1
@57
D=A
@SP
A=M
M=D
@SP
M=M+1
@31
D=A
@SP
A=M
M=D
@SP
M=M+1
@53
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
M=M-1
@SP
A=M
D=M
@SP
M=M-1
A=M
M=M+D
@SP
M=M+1
@112
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
M=M-1
@SP
A=M
D=M
@SP
M=M-1
A=M
M=M-D
@SP
M=M+1
@SP
M=M-1
A=M
M=-M
@SP
M=M+1
@SP
M=M-1
@SP
A=M
D=M
@SP
M=M-1
A=M
M=M&D
@SP
M=M+1
@82
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
M=M-1
@SP
A=M
D=M
@SP
M=M-1
A=M
M=M|D
@SP
M=M+1
@SP
M=M-1
A=M
M=!M
@SP
M=M+1
