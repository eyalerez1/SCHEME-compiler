/* scheme/is_list.asm
 * Take pointers to a Scheme object, and places in R0 either 0 or 1
 * (long, not Scheme integer objects or Scheme boolean objets),
 * depending on whether the argument is a pair.
 * 
 * Programmer: Liraz Reichenstein, 2017
 */

 IS_LIST:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(0));
  CMP(IND(R0), T_PAIR);
  JUMP_NE(IS_LIST_NOT_A_LIST);
  MOV(R0, INDD(R0,2));
IS_LIST_LOOP:
  CMP(R0,IMM(1001));
  JUMP_EQ(INDEED_LIST);
  PUSH(R0);
  CALL(IS_SOB_PAIR);
  CMP(R0,IMM(1));
  POP(R0);
  JUMP_NE(IS_LIST_NOT_A_LIST);
  MOV(R0,INDD(R0,2));
  JUMP(IS_LIST_LOOP);
IS_LIST_NOT_A_LIST:
  MOV(R0,IMM(0));
  JUMP(IS_LIST_FINISH);
INDEED_LIST:
  MOV(R0,IMM(1));
IS_LIST_FINISH:
  POP(FP);
  RETURN;
