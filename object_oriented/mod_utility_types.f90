MODULE mod_utility_types
   IMPLICIT none
   PRIVATE 
   
   TYPE, PUBLIC :: any_object
      CHARACTER(len=:), ALLOCATABLE :: description
      CLASS(*), ALLOCATABLE :: value(:)
      INTEGER, ALLOCATABLE :: shape(:)
   END TYPE
   
   INTERFACE any_object
      MODULE PROCEDURE create_scalar
      MODULE PROCEDURE create_array
   END INTERFACE
CONTAINS
   TYPE(any_object) FUNCTION create_scalar(description, value)
      CHARACTER(len=*), INTENT(in) :: description
      CLASS(*), OPTIONAL, INTENT(in) :: value
      
      create_scalar % description = trim(description)
      IF ( present(value) ) THEN
         ALLOCATE( create_scalar % value(1), source = value ) 
      END IF
   END FUNCTION
   TYPE(any_object) FUNCTION create_array(description, value, shape)
      CHARACTER(len=*), INTENT(in) :: description
      CLASS(*), INTENT(in) :: value(:)
      INTEGER, OPTIONAL, INTENT(in) :: shape(:) ! really ony needed for
                                                ! target rank > 1
      create_array % description = trim(description)
      ALLOCATE( create_array % value, source = value)
      IF ( present(shape) ) THEN
         create_array % shape = shape
      END IF
   END FUNCTION
END MODULE
