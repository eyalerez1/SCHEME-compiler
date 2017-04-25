/* scheme/make_sob_fraction.asm
 * Takes two integers, and place the corresponding Scheme object in R0
 * 
 * Programmer: Liraz Reichenstein, 2017
 */

 MAKE_SOB_FRAC:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(FPARG(1));
  PUSH(FPARG(0));
  PUSH(IMM(2));
  PUSH(IMM(0));
  CALL(GCD);
  DROP(4);
  MOV(R1,FPARG(0));
  DIV(R1,R0);
  MOV(R2,FPARG(1));
  DIV(R2,R0);
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  CMP(R2,IMM(0));
  JUMP_GT(MAKE_FRAC_FINISH);
  MUL(R2,IMM(-1));
  MUL(R1,IMM(-1));
MAKE_FRAC_FINISH:
  MOV(IND(R0), T_FRAC);
  MOV(INDD(R0, 1), R1);
  MOV(INDD(R0, 2), R2);
  POP(FP);
  RETURN;
