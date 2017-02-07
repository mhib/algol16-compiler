#include <cstdio>
#include <cstring>
#include <cstdlib>
#include <cstdint>
#include <cinttypes>
#include <endian.h>
#include <vector>

int main() {
    FILE * reader;
    char * line = NULL;
    size_t len = 0;
    ssize_t read;
    std::vector<int16_t> codeArray;
    reader = fopen("output.txt", "r");
    while ((read = getline(&line, &len, reader)) != -1) {
	  codeArray.push_back(be16toh(atoi(line)));
    }

    fclose(reader);


    FILE * writer;
    writer = fopen("prog.sextium", "w");
    fwrite(&codeArray[0], 2, codeArray.size(), writer);
    fclose(writer);
    return(0);
}
