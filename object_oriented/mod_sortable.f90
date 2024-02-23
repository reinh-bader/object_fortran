MODULE mod_sortable
   USE mod_utility_types, initialize => any_object
   IMPLICIT none
    
   TYPE, ABSTRACT :: sortable
   CONTAINS
      PROCEDURE(compare), DEFERRED :: less_than
      GENERIC :: OPERATOR(<) => less_than
   END TYPE
   
   ABSTRACT INTERFACE
      PURE LOGICAL FUNCTION compare(s1, s2)
         IMPORT :: sortable
         CLASS(sortable), INTENT(in) :: s1, s2 ! dispatch is via the first argument
      END FUNCTION
   END INTERFACE
   
   INTERFACE sortable
      PROCEDURE :: create_sortable
   END INTERFACE
   INTERFACE
      MODULE FUNCTION create_sortable(init) RESULT(r)
        CLASS(sortable), ALLOCATABLE :: r
        TYPE(initialize), INTENT(in) :: init
      END FUNCTION
   END INTERFACE
END MODULE
