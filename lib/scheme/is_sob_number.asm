/* scheme/is_sob_number.asm
 * Take pointers to a Scheme object, and places in R0 either 0 or 1
 * (long, not Scheme integer objects or Scheme boolean objets),
 * depending on whether the argument is integer.
 * 
 * Programmer: NOT Mayer Goldberg, 2017
 */

 IS_SOB_NUMBER:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(0));
  CMP(IND(R0), T_INTEGER);
  JUMP_EQ(L_IS_SOB_NUMBER_TRUE);
  CMP(IND(R0), T_FRAC);
  JUMP_EQ(L_IS_SOB_NUMBER_TRUE);
  MOV(R0, IMM(0));
  JUMP(L_IS_SOB_NUMBER_EXIT);
 L_IS_SOB_NUMBER_TRUE:
  MOV(R0, IMM(1));
 L_IS_SOB_NUMBER_EXIT:
  POP(FP);
  RETURN;


