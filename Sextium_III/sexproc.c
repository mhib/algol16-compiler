/*
 * sexproc.c - implementation of the Sextium® III processor
 */

#include<endian.h>  // be16toh, htobe16
#include"sexproc.h"

struct retval run(machine_word memory[MEMORY_SIZE],
                  enum retcode syscall(struct processor_core *proc)) {
   struct processor_core proc = {
      .accx = 0,
      .ar = 0,
      .dr = 0,
      .pc = 0,
      .idx = 0,
      .buf = 0
   };
   struct retval ret = {
      .errcode = STOP_OTHER,
      .steps = 0,
      .errcode_param = 0,
      .final_pc = 0,
      .final_idx = 0
   };
   enum instruction instr;
   machine_word tmp_reg;

   for (;;) {
      ret.steps++;
      if (proc.idx == 0) {
         ret.final_pc = proc.pc;
         if (proc.pc >= MEMORY_SIZE) {
            ret.errcode = STOP_MEMFAULT;
            ret.errcode_param = proc.pc;
            ret.final_idx = 3 - proc.idx;
            return ret;
         } else proc.buf = be16toh(memory[proc.pc++]);
         proc.idx = 3;
      } else proc.idx--;
      instr = (proc.buf >> (proc.idx * INSTLEN)) & ((1<<INSTLEN)-1);
      switch (instr) {
         case NOP_INSTR:
            break;
         case SYSCALL_INSTR:
            switch (ret.errcode = syscall(&proc)) {
               case SYSCALL_OK:
                  break;
               default:
                  ret.errcode = STOP_OTHER;
               case STOP_SYSCALLERR:
                  ret.errcode_param = proc.accl;
               case STOP_OK:
                  ret.final_idx = 3 - proc.idx;
                  return ret;
            }
            break;
         case LOAD_INSTR:
            if (proc.ar >= MEMORY_SIZE) {
               ret.errcode = STOP_MEMFAULT;
               ret.errcode_param = proc.ar;
               ret.final_idx = 3 - proc.idx;
               return ret;
            } else proc.accl = be16toh(memory[proc.ar]);
            break;
         case STORE_INSTR:
            if (proc.ar >= MEMORY_SIZE) {
               ret.errcode = STOP_MEMFAULT;
               ret.errcode_param = proc.ar;
               ret.final_idx = 3 - proc.idx;
               return ret;
            } else memory[proc.ar] = htobe16(proc.accl);
            break;
         case SWAPA_INSTR:
            tmp_reg = proc.ar;
            proc.ar = proc.accl;
            proc.accl = tmp_reg;
            break;
         case SWAPD_INSTR:
            tmp_reg = proc.dr;
            proc.dr = proc.accl;
            proc.accl = tmp_reg;
            break;
         case BRANCHZ_INSTR:
            if (proc.accl == 0) {
               proc.pc = proc.ar;
               proc.idx = 0;
            }
            break;
         case BRANCHN_INSTR:
            if (proc.accl < 0) {
               proc.pc = proc.ar;
               proc.idx = 0;
            }
            break;
         case JUMP_INSTR:
            proc.pc = proc.accl;
            proc.idx = 0;
            break;
         case CONST_INSTR:
            if (proc.pc >= MEMORY_SIZE) {
               ret.errcode = STOP_MEMFAULT;
               ret.errcode_param = proc.pc;
               ret.final_idx = 3 - proc.idx;
               return ret;
            } else proc.accl = be16toh(memory[proc.pc++]);
            break;
         case ADD_INSTR:
            proc.accx += proc.dr;
            break;
         case SUB_INSTR:
            proc.accx -= proc.dr;
            break;
         case MUL_INSTR:
            proc.accx *= proc.dr;
            break;
         case DIV_INSTR:
            if (proc.dr == 0) {
               ret.errcode = STOP_DIVZEROERR;
               ret.final_idx = 3 - proc.idx;
               return ret;
            } else {
               proc.acch = proc.accl % proc.dr;
               proc.accl /= proc.dr;
            }
            break;
         case SHIFT_INSTR:
            if (proc.dr < 0) {
                 proc.accx >>= -proc.dr;
            }
            else proc.accx <<=  proc.dr;
            break;
         case NAND_INSTR:
            proc.accl = ~(proc.accl & proc.dr);
            break;
         default:
            ret.errcode = STOP_INSTRCODE;
            ret.errcode_param = instr;
            ret.final_idx = 3 - proc.idx;
            return ret;
      }
   }
}

// twi 2016/04/05 vim:fenc=utf8:ts=3:cc=73,74,75,76,77,78,79,80:
