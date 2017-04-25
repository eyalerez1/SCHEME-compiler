/* scheme/is_sob_fraction.asm
 * Take pointers to a Scheme object, and places in R0 either 0 or 1
 * (long, not Scheme integer objects or Scheme boolean objets),
 * depending on whether the argument is integer.
 * 
 * Programmer: Liraz Reichenstein, 2017
 */

 IS_SOB_FRAC:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(0));
  CMP(IND(R0), T_FRAC);
  JUMP_EQ(L_IS_SOB_FRACTION_TRUE);
  MOV(R0, IMM(0));
  JUMP(L_IS_SOB_FRACTION_EXIT);
 L_IS_SOB_FRACTION_TRUE:
  MOV(R0, IMM(1));
 L_IS_SOB_FRACTION_EXIT:
  POP(FP);
  RETURN;


