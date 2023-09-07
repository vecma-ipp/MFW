#include "filetools.h"
#include <stdio.h>
#include <stdlib.h>

void dealloc_charbuf(char **data)
{
  free(*data);
}


void file2char(const char *filename, char **data, int *size)
{
  FILE *f = NULL;
  //char* data = NULL;
  const int segment = 1024*1024;
  int tmpsize = 1*segment;
  int oldsize = 0;
  size_t ret_size = 0;
  int stop = 0;

  f = fopen(filename,"r");
  *data = malloc(tmpsize*sizeof(char));

  while(!stop)  
    {
      ret_size += fread(&((*data)[oldsize]), sizeof(char), segment, f);

      // buffer too small => double size
      if (ret_size == tmpsize)
	{
	  oldsize = tmpsize;
	  tmpsize *= 2;
	  //printf("Increase string size to %d chars\n",tmpsize);
	  *data = realloc(*data, tmpsize*sizeof(char));
	}
      // buffer to large => shrink to exact size
      else 
	{
	  assert(ret_size<tmpsize);
	  *data = realloc(*data,(ret_size+1)*sizeof(char));
	  (*data)[ret_size] = '\0';
	  stop = 1;
	}
    }

  //printf("Just read %d chars\n",ret_size);
  fclose(f);

  *size = ret_size+1;
}


void char2file(const char *filename, const char *data)
{
  FILE *f = NULL;
  size_t size = 0;

  f = fopen(filename,"w");

  size = fwrite(data, sizeof(char), strlen(data), f);

  //printf("Number of written chars = %d\n",size);
  fclose(f);
}



void dealloc_bytebuf(unsigned char **data)
{
  free(*data);
}


void file2byte(const char *filename, unsigned char **data, int *size)
{
  FILE *f = NULL;
  //unsigned char* data = NULL;
  const int segment = 1024*1024;
  int tmpsize = segment;
  int oldsize = 0;
  size_t ret_size = 0;
  int stop = 0;

  // //DEBUG part: 5 lines
  // fprintf(stdout, "Filename = %s \n", filename);
  // int MAX_PATH_LENGTH = 128;
  // char* path[MAX_PATH_LENGTH];
  // getcwd(path, MAX_PATH_LENGTH);
  // fprintf(stdout, "Current Directory = %s \n", path);  

  f = fopen(filename,"r");
  //DEBUG part: 2 lines
  if(f == NULL)
  {fprintf(stdout, "Couldn't open the file! \n");}
  *data = malloc(tmpsize*sizeof(unsigned char));

  while(!stop)  
    {
      // can replace segment by tmpsize-oldsize...
      ret_size += fread(&((*data)[oldsize]), sizeof(unsigned char), segment, f);

      // buffer too small => double size
      if (ret_size == tmpsize)
	{
	  oldsize = tmpsize;
	  tmpsize += segment;
	  printf("Increase buffer size to %d KB\n",tmpsize/1024); //was commented out
	  *data = realloc(*data, tmpsize*sizeof(unsigned char));
	}
      // buffer to large => shrink to exact size
      else 
	{
	  assert(ret_size<tmpsize);
	  *data = realloc(*data,ret_size*sizeof(unsigned char));
	  stop = 1;
	}
    }

  printf("Just read %.2f KB\n",(double)ret_size/1024); //was commneted out
  fclose(f);

  *size = ret_size;
}



void byte2file(const char *filename, const unsigned char *data, int size)
{
  FILE *f = NULL;
  size_t ret_size = 0;

  //printf(">byte2file: opening file\n"); //DEBUG
  f = fopen(filename,"w");
  
  ret_size = fwrite(data, sizeof(unsigned char), size*sizeof(unsigned char), f);

  //printf(">byte2file: file is written\n"); //DEBUG
  //DEBUG: next line is originally commented out!
  //printf("Number of written KB = %.2f\n",(double)ret_size/1024);
  fclose(f);
}



#ifdef CTEST
int main(int *argc, char **argv)
{
  unsigned char *buf = NULL;
  char *data = NULL;
  int size = 0;

  file2byte("equilibrium.cpo", &buf, &size);
  byte2file("equil_byte.cpo", buf, size);


  size = 0;
  file2char("equilibrium.cpo", &data, &size);
  //printf("%s\n",data);
  char2file("equil_char.cpo", data);

  return 0;
}
#endif
