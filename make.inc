ifndef COMPILER
COMPILER=GCC
endif

ifeq ($(COMPILER),Intel)
F90=ifx
CC=icx
FFLAGS=-standard-semantics
DEBUG=-g -traceback -check all
FPP=-fpp
FOPT=-O3 -xAVX  # for Sandy Bridge
#FOPT=-O3 -xAVX2  # for Haswell
#FOPT=-O3 -xSKYLAKE-AVX512  
#FOPT=-O3 -xSSE2
#FOPT=-O3
FPAR=-parallel
F_IEEE=-assume fpe_summary -assume ieee_fpe_flags
# the above is implied by -standard-semantics
#F_IEEE=
SOPT=-shared -fpic
endif

ifeq ($(COMPILER),GCC)
F90=gfortran-13
CC=gcc-13
FFLAGS=
DEBUG=-g -fbacktrace
FPP=-cpp
#FOPT=-O3 -march=opteron -msse3 -funroll-loops 
FOPT=-O3 -march=sandybridge -mavx -funroll-loops 
#FOPT=-Ofast -march=sandybridge -mavx 
#FOPT=-Ofast -march=haswell -mavx2 
#FOPT=-Ofast -march=skylake-avx512 -mtune=skylake-avx512
FPAR=-fopenmp
SOPT=-shared -fpic
endif

ifeq ($(COMPILER),NAG)
F90=nagfor
CC=gcc
FFLAGS=-f2018
#DEBUG=-g -mtrace -C=all
DEBUG=-g -C=all
FPP=-fpp
FOPT=-O3
FPAR=-openmp
F_IEEE=-ieee=full
SOPT=-Wl,-shared -pic
endif

ifeq ($(COMPILER),CRAY)
F90=ftn
CC=cc
FFLAGS=-e m
DEBUG=-g 
endif

ifeq ($(COMPILER),NVidia)
F90=nvfortran
CC=nvc
FFLAGS=
DEBUG=-g 
FOPT=-O3 -tp=haswell
#FOPT=-O3 -tp=skylake
FPP=-Mpreprocess
F_IEEE=-Kieee
# NOTE: the above only impacts precision, not exception handling
SOPT=-shared -fpic
endif

ifeq ($(COMPILER),IBM)
F90=xlf2003_r
CC=xlc_r
FFLAGS=
DEBUG=-g 
FOPT=-O4 -q64 -qhot=vector -qcache=auto -qalias=noaryovrlp
FPP=-qsuffix=cpp=f90
endif
