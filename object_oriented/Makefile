# The ../make.inc file contains possible settings for COMPILER
COMPILER=NAG
include ../make.inc
#uncomment the following line to enable full checking
FOPT+=$(DEBUG) 


all : body.exe exercise_sortable.exe  sorted_list.exe

%.exe : %.f90
	$(F90) $(FFLAGS) $(FOPT) -o $@  $^

%.o : %.f90
	$(F90) $(FFLAGS) $(FOPT) -c -o $@ $^
	
mod_sortable.o : mod_sortable.f90 | mod_utility_types.o
	$(F90) $(FFLAGS) $(FOPT) -c -o $@ $^
	
body.exe : body.f90 mod_utility_types.o
	$(F90) $(FFLAGS) $(FOPT) -o $@ $^

mod_sortable_extensions.o : mod_sortable_extensions.f90 | mod_sortable.o mod_utility_types.o
	$(F90) $(FFLAGS) $(FOPT) -c -o $@ $^

smod_sortable.o : smod_sortable.f90 | mod_sortable_extensions.o mod_utility_types.o
	$(F90) $(FFLAGS) $(FOPT) -c -o $@ $^
	
exercise_sortable.exe : exercise_sortable.f90 mod_utility_types.o mod_sortable.o mod_sortable_extensions.o smod_sortable.o
	$(F90) $(FFLAGS) $(FOPT) -o $@  $^

sorted_list.exe : sorted_list.f90 mod_utility_types.o mod_sortable.o mod_sortable_extensions.o smod_sortable.o
	$(F90) $(FFLAGS) $(FOPT) -o $@  $^
	
object_setup.exe : object_setup.f90 mod_utility_types.o 
	$(F90) $(FFLAGS) $(FOPT) -o $@  $^

clean :
	rm -rf a.out *~ *.o *.exe  *.mod *.smod *.log *.sub my_namelist.txt
