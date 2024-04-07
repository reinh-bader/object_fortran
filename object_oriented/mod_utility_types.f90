MODULE mod_utility_types
  IMPLICIT none
  PRIVATE 

  !
  ! generic container for handling data of arbitrary type and shape
  TYPE, PUBLIC :: any_object
     CHARACTER(len=:), ALLOCATABLE :: description
     CLASS(*), ALLOCATABLE :: value(:)
     INTEGER, ALLOCATABLE :: shape(:)
  END TYPE any_object

  INTERFACE any_object
     MODULE PROCEDURE create_scalar
     MODULE PROCEDURE create_array
  END INTERFACE any_object
CONTAINS
  !
  ! construction of any_object scalars and arrays
  TYPE(any_object) FUNCTION create_scalar(description, value)
    CHARACTER(len=*), INTENT(in) :: description
    CLASS(*), OPTIONAL, INTENT(in) :: value

    create_scalar % description = trim(description)
    IF ( present(value) ) THEN
       ALLOCATE( create_scalar % value(1), source = value ) 
    END IF
  END FUNCTION create_scalar
  TYPE(any_object) FUNCTION create_array(description, value, shape)
    CHARACTER(len=*), INTENT(in) :: description
    CLASS(*), INTENT(in) :: value(:)
    ! shape really only needed for target rank > 1
    INTEGER, OPTIONAL, INTENT(in) :: shape(:)
    
    create_array % description = trim(description)
    ALLOCATE( create_array % value, source = value)
    IF ( present(shape) ) THEN
       create_array % shape = shape
    END IF
  END FUNCTION create_array

END MODULE mod_utility_types
