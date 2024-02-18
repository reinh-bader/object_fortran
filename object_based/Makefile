COMPILER=Intel
include ../make.inc
#FOPT+=$(DEBUG)


all : polynomial.exe sorted_list.exe

%.exe : %.f90
	$(F90) $(FFLAGS) $(FOPT) -o $@  $^

%.o : %.f90
	$(F90) -c -o $@ $^


clean :
	rm -rf a.out *~ *.o *.exe  *.mod *.log
