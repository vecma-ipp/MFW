#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>


void dealloc_charbuf(char **data);

void file2char(const char *filename, char **data, int *size);
void char2file(const char *filename, const char *data);

void dealloc_bytebuf(unsigned char **data);

void file2byte(const char *filename, unsigned char **data, int *size);
void byte2file(const char *filename, const unsigned char *data, int size);

