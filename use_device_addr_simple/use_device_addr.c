#include <stdio.h>
#include <stdlib.h>

int main (int argc, char *argv[])
{
  float *A = (float *) malloc ( sizeof ( float ) * 10 );
  
  printf("%p\n", A); // print A device addr

  #pragma omp target enter data map ( to: A[0:10] ) 

  #pragma omp target data use_device_ptr ( A )
  { printf("%p\n", A); } // print A device addr 

  printf("%p\n", A);  // print A host addr
} 
