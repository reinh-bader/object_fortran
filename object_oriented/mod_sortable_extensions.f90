MODULE mod_sortable_extensions
   USE mod_sortable
   IMPLICIT none
   
   PRIVATE 
   
   TYPE, PUBLIC, EXTENDS(sortable) :: sortable_string
      CHARACTER(len=:), ALLOCATABLE :: string
   CONTAINS
      PROCEDURE :: less_than => less_than_string
   END TYPE
CONTAINS
   PURE LOGICAL FUNCTION less_than_string(s1, s2)
      CLASS(sortable_string), INTENT(in) :: s1
      CLASS(sortable), INTENT(in) :: s2
      
      SELECT TYPE (s2)
      CLASS IS (sortable_string)
         IF ( allocated(s1%string) .AND. allocated(s2%string) ) THEN
            less_than_string = llt(s1%string,s2%string)
         ELSE
            less_than_string = .false.    
        END IF
     CLASS default
        less_than_string = .false.
     END SELECT
   END FUNCTION   
END MODULE
