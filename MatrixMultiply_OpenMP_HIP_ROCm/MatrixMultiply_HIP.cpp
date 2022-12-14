#include <omp.h>

#ifdef __HIP_PLATFORM_HCC__
#include <hip/hip_runtime.h>
#else
#include <cuda_runtime_api.h>
#include <cuda.h>
#include <cublas_v2.h>
#define hipLaunchKernelGGL(F,G,B,M,S,...) F<<<G,B,M,S>>>(__VA_ARGS__)
#define hipDeviceSynchronize cudaDeviceSynchronize
#endif

#include <stdio.h>

#ifdef __cplusplus
extern "C"
  { 
#endif
void MatrixMultiply_HIP_Launch
       ( double *d_A, double *d_B, double *d_C, int nV );
void MatrixMultiply_BLAS 
       ( double *d_A, double *d_B, double *d_C, int nV );
#ifdef __cplusplus
  }
#endif

// GPU kernel to multiply matrices
__global__ void matrix_multiply(double *a, double *b, double *c, int N)
{
    int row    = blockDim.x * blockIdx.x + threadIdx.x;
    int column = blockDim.y * blockIdx.y + threadIdx.y;

    if (row < N && column < N)
    {
        // dot product of row, column
        double element = 0.0;
        for(int i=0; i<N; i++){
          //element += a[row * N + i] * b[i * N + column];
          element += a[row + N * i] * b[i + N * column];
        }
        
        //c[row * N + column] = element;
        c[row + N * column] = element;
    }
}

void MatrixMultiply_HIP_Launch  ( double *d_A, double *d_B, double *d_C, int nV )
  {

  // Set execution configuration parameters
  //    threads_per_block: number of GPU threads per grid block
  //    blocks_in_grid   : number of blocks in grid
  //    (These are c structs with 3 member variables x, y, x)
  dim3 threads_per_block ( 16, 16, 1 );
  dim3 blocks_in_grid ( ceil( float(nV) / threads_per_block.x ), 
                          ceil( float(nV) / threads_per_block.y ), 1 );

  
  // Launch kernel
  double start = omp_get_wtime ( );
  hipLaunchKernelGGL ( matrix_multiply, blocks_in_grid, threads_per_block, 0, 
                       0, d_A, d_B, d_C, nV );
  hipDeviceSynchronize ( );
  
  /* 
  for (int i = 0; i < nV; i++) {
    for (int j = 0; j < nV; j++) {
      //d_C[i * nV + j] = 0;
      d_C[i + nV * j] = 0;
      for (int k = 0; k < nV; k++) {
        //d_C[i * nV + j] += d_A[i * nV + k] * d_B[k * nV + j];
        d_C[i + nV * j] += d_A[i + nV * k] * d_B[k + nV * j];
        }
      }
    }
  */
  
  }

/*
void MatrixMultiply_BLAS  ( double *d_A, double *d_B, double *d_C, int nV )
  {
  
  cublasHandle_t Handle;
  cublasStatus_t Status;
  double Alpha, Beta;

  double *A, *B, *C;
  
  A = ( double * ) malloc ( nV * nV * sizeof ( double ) );
  B = ( double * ) malloc ( nV * nV * sizeof ( double ) );
  C = ( double * ) malloc ( nV * nV * sizeof ( double ) );

  cudaMemcpy ( A, d_A, nV * nV * sizeof ( double ), cudaMemcpyDeviceToHost );
  cudaMemcpy ( B, d_B, nV * nV * sizeof ( double ), cudaMemcpyDeviceToHost );
  cudaMemcpy ( C, d_C, nV * nV * sizeof ( double ), cudaMemcpyDeviceToHost );
  
  printf ( "Enter A, B, C\n" );
  for ( int iV = 0; iV < nV * nV; iV++ )
    printf ( "%f %f %f \n", A [ iV ], B [ iV ], C [ iV ] );

  Alpha = 1.0;
  Beta = 0.0;
  
  printf ( "<<<<<<<<<<< MM_BLAS >>>>>>>>>>>>>>>>> \n" );
  Status = cublasCreate_v2 ( &Handle );
  if ( Status != CUBLAS_STATUS_SUCCESS) 
    printf ("CUBLAS initialization failed\n");
  
  Status = cublasDgemm ( Handle, CUBLAS_OP_N, CUBLAS_OP_N, nV, nV, nV, &Alpha, 
                    d_A, nV, d_B, nV, &Beta, d_C, nV );
  if ( Status != CUBLAS_STATUS_SUCCESS )
    printf("cublasDgemm failed with code %d\n", Status);
  
  cudaMemcpy ( C, d_C, nV * nV * sizeof ( double ), cudaMemcpyDeviceToHost );
  printf ( "Exit, C\n" );
  for ( int iV = 0; iV < nV * nV; iV++ )
    printf ( "%f \n", C [ iV ] );

  cublasDestroy ( Handle );
  }
*/
