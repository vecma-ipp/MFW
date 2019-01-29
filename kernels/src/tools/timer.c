#include "timer.h"

#ifdef TEST
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#endif


void getMillis(unsigned long long *t)
{
  int err;
  struct timeval tv;
  struct timezone tz;

  err = gettimeofday(&tv, &tz);
  *t = tv.tv_sec*1e3 + tv.tv_usec/1e3;
}


#ifdef TEST
int main(int argc, char **argv)
{
  unsigned long long t0,t1,tinit,tend;
  int size = 1000;
  double *vec0 = NULL;
  double **mat0 = NULL;
  double *vec1 = NULL;
  double sum = 0.;
  int i,j;

  if (argc > 1)
    {
      size = atoi(argv[1]);
      printf("Change array size to %d\n",size);
    }

  getMillis(&tinit);
  
  getMillis(&t0);
  vec0 = malloc(size*sizeof(double));
  mat0 = malloc(size*sizeof(double *));
  for (i=0; i<size; i++)
    mat0[i] = malloc(size*sizeof(double));
  vec1 = malloc(size*sizeof(double));
  getMillis(&t1);
  printf("alloc in %llu ms\n",(t1-t0));
  
  getMillis(&t0);
  for (i=0; i<size; i++)
    {
      vec0[i] = i*2.0;
      for (j=0; j<size; j++)
	mat0[i][j] = 10./(double)(1+i) * sin(2.0*j);
    }
  getMillis(&t1);
  printf("init in %llu ms\n",(t1-t0));

  getMillis(&t0);
  for (i=0; i<size; i++)
    {
      vec1[i] = 0.;
      for (j=0; j<size; j++)
	vec1[i] += vec0[j] * mat0[i][j];
      sum += vec1[i];
    }
  getMillis(&t1);
  printf("sum = %f (in %llu ms)\n",sum,(t1-t0));

  getMillis(&tend);
  printf("total time = %llu ms\n",(tend-tinit));

  return 0;
}
#endif
