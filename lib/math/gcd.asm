GCD:
PUSH(FP);
MOV(FP,SP);
PUSH(R3);
CMP(FPARG(1),IMM(2));
JUMP_NE(LERROR_INCORRECT_NUM_OF_ARGS);
MOV(R1, FPARG(2));
MOV(R2,FPARG(3));
CMP(R2,IMM(0));
JUMP_EQ(LERROR_SECOND_ARG_CANNOT_BE_ZERO);
CMP(R1,IMM(0));
JUMP_EQ(GCD_EXIT);
GCD_LOOP:
MOV(R3,R1);
REM(R3,R2);
CMP(R3,IMM(0));
JUMP_EQ(GCD_EXIT);
MOV(R1,R2);
MOV(R2,R3);
JUMP(GCD_LOOP);
GCD_EXIT:
MOV(R0,R2);
POP(R3);
POP(FP);
RETURN;