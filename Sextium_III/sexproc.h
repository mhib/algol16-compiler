/*
 * sexproc.h - specification of the Sextium® III processor 
 */

#include<stdint.h>   // int16_t, int32_t, uint16_t
#include<endian.h>   // __BYTE_ORDER, __LITTLE_ENDIAN

// Machine word is 16 bit long, big endian
typedef int16_t  machine_word;
typedef int32_t  double_machine_word;
typedef uint16_t unsigned_machine_word;

// instruction length in bits
#define INSTLEN 4

// memory size (number of machine words)
#define MEMORY_SIZE 65536

// instruction codes
enum instruction {
   NOP_INSTR,      // 0
   SYSCALL_INSTR,  // 1
   LOAD_INSTR,     // 2
   STORE_INSTR,    // 3
   SWAPA_INSTR,    // 4
   SWAPD_INSTR,    // 5
   BRANCHZ_INSTR,  // 6
   BRANCHN_INSTR,  // 7
   JUMP_INSTR,     // 8
   CONST_INSTR,    // 9
   ADD_INSTR,      // a
   SUB_INSTR,      // b
   MUL_INSTR,      // c
   DIV_INSTR,      // d
   SHIFT_INSTR,    // e
   NAND_INSTR      // f
};

// Collection of processor registers
struct processor_core {
   union {
      struct {
#if __BYTE_ORDER == __LITTLE_ENDIAN
         machine_word     accl;  // lower 16 bits of the accumulator
         machine_word     acch;  // upper 16 bits of the accumulator
#else
         machine_word     acch;  // upper 16 bits of the accumulator
         machine_word     accl;  // lower 16 bits of the accumulator
#endif
      };
      double_machine_word accx;  // full 32 bits of the accumulator
   };
   unsigned_machine_word  ar;    // address register
   machine_word           dr;    // data register
   unsigned_machine_word  pc;    // program counter
   unsigned_machine_word  idx;   // instruction index in the buffer
   unsigned_machine_word  buf;   // instruction buffer
};

// Return codes
enum retcode {
   SYSCALL_OK,      // 0
   STOP_OK,         // 1
   STOP_SYSCALLERR, // 2
   STOP_DIVZEROERR, // 3
   STOP_MEMFAULT,   // 4
   STOP_INSTRCODE,  // 5
   STOP_OTHER       // 6
};

// Return value
struct retval {
   enum retcode errcode;
   unsigned steps;
   unsigned errcode_param;
   unsigned_machine_word final_pc;
   unsigned_machine_word final_idx;
};

// Run a program from memory
struct retval run(machine_word memory[MEMORY_SIZE],
                  enum retcode syscall(struct processor_core *));

// twi 2016/04/05 vim:fenc=utf8:ts=3:cc=73,74,75,76,77,78,79,80:
