/* scheme/write_sob_symbol.asm
 * Take a pointer to a Scheme string object, and 
 * prints (to stdout) the character representation
 * of that object.
 * 
 * Programmer:  NOT Mayer Goldberg, 2010
 */

 WRITE_SOB_SYMBOL:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(R1);
  PUSH(R2);
  PUSH(R3);

  MOV(R0, FPARG(0));
  MOV(R0,INDD(R0,1));
  MOV(R1, INDD(R0, 1));
  MOV(R2, R0);
  ADD(R2, IMM(2));

 L_WSSYM_LOOP:
  CMP(R1, IMM(0));
  JUMP_EQ(L_WSSYM_EXIT);

  PUSH(IND(R2));
  CALL(PUTCHAR);
  DROP(1);

  INCR(R2);
  DECR(R1);
  JUMP(L_WSSYM_LOOP);

 L_WSSYM_EXIT:
  POP(R3);
  POP(R2);
  POP(R1);
  POP(FP);
  RETURN;

