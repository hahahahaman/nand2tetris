// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel;
// the screen should remain fully black as long as the key is pressed. 
// When no key is pressed, the program clears the screen, i.e. writes
// "white" in every pixel;
// the screen should remain fully clear as long as no key is pressed.

// Put your code here.

// @1
// D=A // D=1
// @0
// M=D // M[0] = 1
// A=M // A= M[0] =1
// M=M+1 // M[1] = M[1]+1


(LOOP)
    @24576 // keyboard input location
    D=M
    @FILL
    D;JGT // M[24576] > 0, key pressed

    @UNFILL
    D;JEQ // M[24576] = 0, key not pressed

    @LOOP // goto start
    0;JMP
(FILL)
    @i
    M=1

    // black color
    @0
    D=!A
    @blackcolor
    M=D

    // screen memory starting point
    @16384
    D=A
    @screen
    M=D
    (BLACK)
       @i
       D=M
       @256
       D=D-A
       @BLACKEND
       D;JGT // i-256 > 0

       @j
       M=1
       (BLACKINNER)
           @j
           D=M
           @32
           D=D-A
           @BLACKINNEREND
           D;JGT // j-32 >0

           @blackcolor
           D=M
           @screen
           A=M // A = M[screen]
           M=D

           @j
           M=M+1

           @screen
           M=M+1

           @BLACKINNER // goto blackinner loop start
           0;JMP
       (BLACKINNEREND)

       @i
       M=M+1

       @BLACK
       0;JMP
    (BLACKEND)
       @LOOP
       0;JMP // //goto start
(UNFILL)
    @i
    M=1

    // white color
    @0
    D=A
    @whitecolor
    M=D

    // screen memory starting point
    @16384
    D=A
    @screen
    M=D
    (WHITE)
       @i
       D=M
       @256
       D=D-A
       @WHITEEND
       D;JGT // i-256 > 0

       @j
       M=1
       (WHITEINNER)
           @j
           D=M
           @32
           D=D-A
           @WHITEINNEREND
           D;JGT // j-32 >0

           @whitecolor
           D=M
           @screen
           A=M // A = M[screen]
           M=D

           @j
           M=M+1

           @screen
           M=M+1

           @WHITEINNER // goto blackinner loop start
           0;JMP
       (WHITEINNEREND)

       @i
       M=M+1

       @WHITE
       0;JMP
    (WHITEEND)
       @LOOP
       0;JMP // //goto start
