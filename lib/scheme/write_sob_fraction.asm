/* scheme/write_sob_fraction.asm
 * Take a pointer to a Scheme integer object, and 
 * prints (to stdout) the character representation
 * of that object.
 * 
 * Programmer: NOT Mayer Goldberg, 2010
 */

 WRITE_SOB_FRAC:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(0));
  MOV(R0, INDD(R0, 1));
  PUSH(R0);
  CALL(WRITE_INTEGER);
  DROP(1);
  PUSH(IMM('/'));
  CALL(PUTCHAR);
  DROP(1);
  MOV(R0, FPARG(0));
  MOV(R0, INDD(R0, 2));
  PUSH(R0);
  CALL(WRITE_INTEGER);
  DROP(1);
  POP(FP);
  RETURN;

