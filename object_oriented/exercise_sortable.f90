PROGRAM exercise_sortable
   USE mod_sortable
   CLASS(sortable), ALLOCATABLE :: s1, s2

   s1 = sortable( initialize("sortable_string", "angle") )
   s2 = sortable( initialize("sortable_string", "briefcase") )
   
   write(*,*) s1 < s2
   
END PROGRAM
   
