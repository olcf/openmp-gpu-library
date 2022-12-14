
ifeq ($(MACHINE), Cray_CCE)
  FC      = ftn -fopenmp -eF
  CC      = cc -fopenmp
  DC      = CC -x hip
  INCLUDE = -I${ROCM_PATH}/include
  LIBRARY = -L${ROCM_PATH}/lib -lhipblas -lamdhip64
endif

ifeq ($(MACHINE), POWER_XL)
  FC      = mpif90 -qsmp=omp -qoffload -qpreprocess
  CC      = xlc -qsmp=omp
  DC      = nvcc -x cu
  INCLUDE = -I${OLCF_CUDA_ROOT}/include
  LIBRARY = -L${OLCF_CUDA_ROOT}/lib64 -lcudart -lcublas -lstdc++
endif

%.o : %.cpp
	$(DC) -D${MACHINE} ${INCLUDE} -c $<

%.o : %.f90
	$(FC) -D${MACHINE} -c $<

MPI_Send_Recv_1: \
  MPI_Send_Recv_1.o
	$(FC) MPI_Send_Recv_1.o \
	      ${LIBRARY} \
	  -o MPI_Send_Recv_1_$(MACHINE) 

MPI_Send_Recv_2: \
  MPI_Send_Recv_2.o
	$(FC) MPI_Send_Recv_2.o \
	      ${LIBRARY} \
	  -o MPI_Send_Recv_2_$(MACHINE) 

MPI_Send_Recv_.o: \
  MPI_Send_Recv_1.f90

MPI_Send_Recv_2.o: \
  MPI_Send_Recv_2.f90

clean:
	rm -f *.mod *.o *.acc.*
