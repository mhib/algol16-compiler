/*
 * sextium.c - implementation of the Sextium® III emulator
 */

#include<stdio.h>   // fopen, fclose, fread, ferror, feof,
                    // fputs, fprintf, stderr, printf, scanf
#include<stdlib.h>  // calloc, free
#include<errno.h>   // errno
#include<string.h>  // strerror
#include"sexproc.h"
#include"sextium.h"

machine_word *initialize_memory(char *filename) {
   machine_word *memory = calloc(MEMORY_SIZE, sizeof(machine_word));
   if (memory == NULL) {
      fputs("Error: memory allocation failed\n", stderr);
      return NULL;
   }
   FILE *file_handle = fopen(filename, "r");
   if (file_handle == NULL) {
      fprintf(stderr, "Error opening program file %s: %s (%d)\n",
         filename, strerror(errno), errno);
      free(memory);
      return NULL;
   }
   size_t program_size =
      fread(memory, sizeof(machine_word), MEMORY_SIZE, file_handle);
   if (ferror(file_handle)) {
      fprintf(stderr, "Error reading program file %s: %s (%d)\n",
         filename, strerror(errno), errno);
      free(memory);
      fclose(file_handle);
      return NULL;
   }
   if (!feof(file_handle)) {
      fprintf(stderr, "Error reading program file %s: "
              "file is longer than %d words\n", filename, MEMORY_SIZE);
      free(memory);
      fclose(file_handle);
      return NULL;
   }
   fclose(file_handle);
   fprintf(stderr, "Program %s: loaded into memory (%zu words)\n",
           filename, program_size);
   return memory;
}

enum retcode syscall(struct processor_core *proc) {
   int read_buffer;
   switch (proc->accl) {
      case HALT_SYS:
         return STOP_OK;
      case READ_SYS:
         printf("Input: ");
         scanf("%d", &read_buffer);
         proc->accl = read_buffer;
         return SYSCALL_OK;
      case WRITE_SYS:
         printf("Output: %d\n", proc->dr);
         return SYSCALL_OK;
      default:
         return STOP_SYSCALLERR;
   }
}

int main(int argc, char *argv[]) {
   if (argc != 2) {
      fprintf(stderr, "Wrong number of command-line parameters: %d "
              "instead of 1\nUsage: %s sextium_executable_file\n",
              argc-1, argv[0]);
      return 1;
   }
   machine_word *memory = initialize_memory(argv[1]);
   if (memory == NULL)
      return 1;
   struct retval rv = run(memory, syscall);
   free(memory);
   switch (rv.errcode) {
      case STOP_OK:
         fprintf(stderr, "Program terminated normally after %d steps\n",
                 rv.steps);
         return 0;
      case STOP_SYSCALLERR:
         fprintf(stderr, "Exception: "
                 "Invalid system call code %d at PC %d:%d\n"
                 "Program terminated abnormally after %d steps\n",
                 rv.errcode_param, rv.final_pc, rv.final_idx, rv.steps);
         return 1;
      case STOP_DIVZEROERR:
         fprintf(stderr, "Exception: Division by zero at PC %d:%d\n"
                 "Program terminated abnormally after %d steps\n",
                 rv.final_pc, rv.final_idx, rv.steps);
         return 1;
      case STOP_MEMFAULT:
         fprintf(stderr, "Exception: "
                 "Address %d outside of memory at PC %d:%d\n"
                 "Program terminated abnormally after %d steps\n",
                 rv.errcode_param, rv.final_pc, rv.final_idx, rv.steps);
         return 1;
      case STOP_INSTRCODE:
         fprintf(stderr, "Exception: " 
                 "Wrong instruction code %d at PC %d:%d\n"
                 "Program terminated abnormally after %d steps\n",
                 rv.errcode_param, rv.final_pc, rv.final_idx, rv.steps);
         return 1;
      default:
         fprintf(stderr, "Exception: "
                 "Unspecified emulator malfunction at PC %d:%d\n"
                 "Program terminated abnormally after %d steps\n",
                 rv.final_pc, rv.final_idx, rv.steps);
         return 1;
   }
}

// twi 2016/04/05 vim:fenc=utf8:ts=3:cc=73,74,75,76,77,78,79,80:
