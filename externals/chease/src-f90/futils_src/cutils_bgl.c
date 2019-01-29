//
//   Utilities C functions.
//
//   T.M. Tran, B. McMillan, S. Brunner, CRPP/EPFL.
//   December 2007
//
#include <stdio.h>
#include <string.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/uio.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>

#include <sys/resource.h>
#include <common/bgp_personality.h>
#include <common/bgp_personality_inlines.h>
#include <spi/kernel_interface.h>

static _BGP_Personality_t mybgp;

/* returns memory per core in MBytes */
unsigned bg_coreMB() {
    unsigned procMB, coreMB;
    Kernel_GetPersonality(&mybgp, sizeof(_BGP_Personality_t));
    procMB = BGP_Personality_DDRSizeMB(&mybgp);
    coreMB = procMB/Kernel_ProcessCount();
    return coreMB;
}

/* return maximum memory usage of process in kBytes */
unsigned bg_usedKB() {
    struct rusage usage;
    if (getrusage(RUSAGE_SELF, &usage) != 0)
	return 0;
    return usage.ru_maxrss;
}

double mem_per_core(void)
{
    //  Return current memory usage in MB on BGP.

  return ((double)bg_coreMB());
}

double mem(void)
{

  //*   return ((double)sbrk(0)/1024.0/1024.0);  on BGP

  return ((double)bg_usedKB()/1024.0);
}

double second(void)
{
    // Return elapsed wall time in s.

    struct timeval tp;
    struct timezone tzp;
    int i;
    i = gettimeofday(&tp,&tzp);
    return ( (double) tp.tv_sec + (double) tp.tv_usec * 1.e-6 );
}

double seconds(void)
{
    // Return elapsed wall time in s.

    struct timeval tp;
    struct timezone tzp;
    int i;
    i = gettimeofday(&tp,&tzp);
    return ( (double) tp.tv_sec + (double) tp.tv_usec * 1.e-6 );
}

int fsize(const char *file, unsigned int l)
{
  //   Return size of file in bytes

  struct stat buf;
  stat( file, &buf);
  return (int)buf.st_size;
}

void ftos(const char *file, int *s, char *buf)
{
  //   Put the content of file to a string

  int fd, n;
  fd = open(file, O_RDONLY);
  n = read(fd, buf, (size_t)*s);
  close(fd);
}

void stof(const char *file, int *s, char *buf)
{
  //   Write string to file

  int fd, n;
  fd = creat(file, 0644);
  n = write(fd, buf, (size_t)*s);
  close(fd);
}

void stostdout(int *s, char *buf)
{
  //   Write string to stdout

  int n;
  n = write(1, buf, (size_t)*s);
}

// Read in 1kb chunks in copy_file
#define CHSIZE 1024

void copy_file(char *infile, int *lengthin, char *outfile, int *lengthout) {

  //  Copies a file from one location to another using fread, fwrite.

  char buf[CHSIZE];
  int  err, count=CHSIZE;

  memcpy(buf,infile,CHSIZE*sizeof(char));
  buf[*lengthin <CHSIZE ? *lengthin  : CHSIZE-1]   = '\0';
  FILE *in  = fopen(buf, "rb");
  if (in == NULL) {
      printf("could not open infile\n");  
      return;
  }
  memcpy(buf,outfile,CHSIZE*sizeof(char));
  buf[*lengthout<CHSIZE ? *lengthout : CHSIZE-1]   = '\0';
  FILE *out = fopen(buf,"wb");
  if (out == NULL) {
      fclose(in);
      printf("could not open outfile\n");      
      return;
  }
    
  while (1) {
      err = fread(buf,sizeof(char),count,in);
      fwrite(buf,sizeof(char),err,out);
      if (err!=count) break;
  }
    
  fclose(in);
  fclose(out);
}

void move_file(char *infile, int *lengthin, char *outfile, int *lengthout) {

  //  Moves a file from one location to another using copy_file_ and
  //  unlink (man 2 unlink)

    char buf[CHSIZE];

    copy_file(infile, lengthin, outfile, lengthout);

    memcpy(buf, infile,CHSIZE*sizeof(char));
    buf[*lengthin <CHSIZE ? *lengthin  : CHSIZE-1]   = '\0';
    if( unlink(buf) == -1 ){
	fprintf(stderr, "%s: removing %s\n", strerror(errno), buf);
    }

}
