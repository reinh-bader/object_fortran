# The ../make.inc file contains possible settings for COMPILER
COMPILER=GCC
include ../make.inc
#uncomment the following line to enable full checking
FOPT+=$(DEBUG) 


all : function_call.exe

%.exe : %.f90
	$(F90) $(FFLAGS) $(FOPT) -o $@  $^

%.o : %.f90
	$(F90) $(FFLAGS) $(FOPT) -c -o $@ $^

function_call.exe : function_call.f90 mod_utility_types.o timer.o
	$(F90) $(FFLAGS) $(FOPT) -o $@  $^

body.exe : body.f90 mod_utility_types.o timer.o
	$(F90) $(FFLAGS) $(FOPT) -o $@  $^
	
pbody.exe : pbody.f90 mod_utility_types.o timer.o
	$(F90) $(FFLAGS) $(FOPT) -o $@  $^

clean :
	rm -rf a.out *~ *.o *.exe  *.mod *.smod *.log *.sub finpos.dat my_namelist.txt
