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
      CLASS(*), INTENT(in) :: value
      
      create_scalar % description = description
      ALLOCATE( create_scalar % value(1), source = value ) 
   END FUNCTION
   TYPE(any_object) FUNCTION create_array(description, value, shape)
      CHARACTER(len=*), INTENT(in) :: description
      CLASS(*), INTENT(in) :: value(:)
      INTEGER, OPTIONAL, INTENT(in) :: shape(:) ! really ony needed for
                                                ! target rank > 1
      
      create_array % description = description
      create_array % value = value 
      IF ( present(shape) ) create_array % shape = shape
   END FUNCTION
END MODULE
