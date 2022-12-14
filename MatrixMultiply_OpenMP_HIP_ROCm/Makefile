
ifeq ($(MACHINE), Cray_CCE)
  FC      = ftn -fopenmp -eF
  CC      = cc -fopenmp
  DC      = CC -x hip
  INCLUDE = -I${ROCM_PATH}/include
  LIBRARY = -L${ROCM_PATH}/lib -lhipblas -lamdhip64
endif

ifeq ($(MACHINE), POWER_XL)
  FC      = xlf2008_r -qsmp=omp -qoffload -qpreprocess
  CC      = xlc -qsmp=omp
  DC      = nvcc -x cu
  INCLUDE = -I${OLCF_CUDA_ROOT}/include
  LIBRARY = -L${OLCF_CUDA_ROOT}/lib64 -lcudart -lcublas -lstdc++
endif

%.o : %.cpp
	$(DC) -D${MACHINE} ${INCLUDE} -c $<

%.o : %.f90
	$(FC) -D${MACHINE} -c $<

MatrixMultiply: \
  MatrixMultiply_Program.o MM_OpenMP_Module.o MM_Utilities_Module.o \
  Timer_Form.o
	$(FC) MatrixMultiply_Program.o \
	      MM_GPU_Library_Module.o \
	      MM_HIP_Module.o \
	      MM_OpenMP_Module.o \
	      MM_Utilities_Module.o \
	      Timer_Form.o \
	      MatrixMultiply_HIP.o \
	      ${LIBRARY} \
	  -o MatrixMultiply_$(MACHINE) 

#Timer_Form.o: \
#  Timer_Form.f90

#MM_Utilities_Module.o: \
#  MM_Utilities_Module.f90

MatrixMultiply_Program.o: \
  Timer_Form.o \
  MM_Utilities_Module.o \
  MM_OpenMP_Module.o \
  MM_HIP_Module.o \
  MM_GPU_Library_Module.o \
  MatrixMultiply_HIP.o \
  MatrixMultiply_Program.f90

clean:
	rm -f *.mod *.o *.acc.*
