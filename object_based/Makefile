# The ../make.inc file contains possible settings for COMPILER
COMPILER=NAG
include ../make.inc
#uncomment the following line to enable full checking
#FOPT+=$(DEBUG)


all : polynomial.exe sorted_list.exe

%.exe : %.f90
	$(F90) $(FFLAGS) $(FOPT) -o $@  $^

%.o : %.f90
	$(F90) -c -o $@ $^


clean :
	rm -rf a.out *~ *.o *.exe  *.mod *.log my_namelist.txt
