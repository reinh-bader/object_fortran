COMPILER=Intel
include ../make.inc
#FOPT+=$(DEBUG)


all : body.exe exercise_sortable.exe

%.exe : %.f90
	$(F90) $(FFLAGS) $(FOPT) -o $@  $^

%.o : %.f90
	$(F90) $(FFLAGS) $(FOPT) -c -o $@ $^
	
mod_sortable.o : mod_sortable.f90 | mod_utility_types.o
	$(F90) $(FFLAGS) $(FOPT) -c -o $@ $^
	
body.exe : body.f90 mod_utility_types.o
	$(F90) $(FFLAGS) $(FOPT) -o $@ $^

mod_sortable_extensions.o : mod_sortable_extensions.f90 | mod_sortable.o
	$(F90) $(FFLAGS) $(FOPT) -c -o $@ $^

smod_sortable.o : smod_sortable.f90 | mod_sortable_extensions.o
	$(F90) $(FFLAGS) $(FOPT) -c -o $@ $^
	
exercise_sortable.exe : exercise_sortable.f90 mod_utility_types.o mod_sortable.o mod_sortable_extensions.o smod_sortable.o
	$(F90) $(FFLAGS) $(FOPT) -o $@  $^

clean :
	rm -rf a.out *~ *.o *.exe  *.mod *.smod *.log *.sub
