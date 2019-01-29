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

double mem_(void)
{
    //  Return current memory usage in MB (linux x86 ony!)

    FILE                   *file;
    char                   proc[256];
    int                    mm, rss;

  sprintf(proc,"/proc/%d/statm",(int)getpid());
  if (!(file = fopen(proc,"r"))) {
    return -1;
  }
  fscanf(file,"%d %d",&mm,&rss);
  fclose(file);
  return ((double)rss) * ((double)getpagesize() /1024.0/1024.0);
}

/* #include <sys/resource.h> */
/* double mem_(void) */
/* { */
/*     //  Return current memory usage in MB */

/*   struct rusage usage; */
/*   double memkb; */

/*   if (getrusage(RUSAGE_SELF, &usage) != 0) */
/*     return -1; */
/*   memkb = (double) usage.ru_maxrss; */
/*   return ( memkb/1024.0 ); */
/* } */

double second_(void)
{
    // Return elapsed wall time in s.

    struct timeval tp;
    struct timezone tzp;
    int i;
    i = gettimeofday(&tp,&tzp);
    return ( (double) tp.tv_sec + (double) tp.tv_usec * 1.e-6 );
}

double seconds_(void)
{
    // Return elapsed wall time in s.

    struct timeval tp;
    struct timezone tzp;
    int i;
    i = gettimeofday(&tp,&tzp);
    return ( (double) tp.tv_sec + (double) tp.tv_usec * 1.e-6 );
}

int fsize_(const char *file, unsigned int l)
{
  //   Return size of file in bytes

  struct stat buf;
  stat( file, &buf);
  return (int)buf.st_size;
}

void ftos_(const char *file, int *s, char *buf)
{
  //   Put the content of file to a string

  int fd, n;
  fd = open(file, O_RDONLY);
  n = read(fd, buf, (size_t)*s);
  close(fd);
}

void stof_(const char *file, int *s, char *buf)
{
  //   Write string to file

  int fd, n;
  fd = creat(file, 0644);
  n = write(fd, buf, (size_t)*s);
  close(fd);
}

void stostdout_(int *s, char *buf)
{
  //   Write string to stdout

  int n;
  n = write(1, buf, (size_t)*s);
}

// Read in 1kb chunks in copy_file,
// also use as temporary for file names
#define CHSIZE 1024

void copy_file_(char *infile, int *lengthin, char *outfile, int *lengthout) {

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

void move_file_(char *infile, int *lengthin, char *outfile, int *lengthout) {

  //  Moves a file from one location to another using copy_file_ and
  //  unlink (man 2 unlink)

    char buf[CHSIZE];

    copy_file_(infile, lengthin, outfile, lengthout);

    memcpy(buf, infile,CHSIZE*sizeof(char));
    buf[*lengthin <CHSIZE ? *lengthin  : CHSIZE-1]   = '\0';
    if( unlink(buf) == -1 ){
	fprintf(stderr, "%s: removing %s\n", strerror(errno), buf);
    }

}
