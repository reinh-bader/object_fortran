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
     MODULE PROCEDURE create_array_r2
  END INTERFACE any_object

  !
  ! parameterized functions
  TYPE, PUBLIC :: pfunc_type
     PRIVATE
     PROCEDURE(pfunc), POINTER, NOPASS :: fp => null()
     PROCEDURE(pfunc_array), POINTER, NOPASS :: fp_array => null()
     CLASS(*), ALLOCATABLE :: param
   CONTAINS
     PROCEDURE, PASS, PRIVATE, NON_OVERRIDABLE :: f_scalar, f_array
     GENERIC :: f => f_scalar, f_array
  END type pfunc_type

  ABSTRACT INTERFACE
     PURE REAL FUNCTION pfunc(x, param)
       REAL, INTENT(in) :: x
       CLASS(*), INTENT(in), OPTIONAL :: param
     END FUNCTION pfunc
     PURE FUNCTION pfunc_array(x, param) RESULT(r)
       REAL, INTENT(in) :: x(:)
       REAL :: r(size(x))
       CLASS(*), INTENT(in), OPTIONAL :: param
     END FUNCTION pfunc_array
  END INTERFACE

  INTERFACE pfunc_type
     MODULE PROCEDURE create_pfunc_type
     MODULE PROCEDURE create_pfunc_type_array
  END INTERFACE pfunc_type
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
  TYPE(any_object) FUNCTION create_array_r2(description, value)
    CHARACTER(len=*), INTENT(in) :: description
    CLASS(*), INTENT(in) :: value(:,:)
  
    create_array_r2 % description = trim(description)
    ALLOCATE( create_array_r2 % value(size(value)), source = &
              reshape(value, shape = [ size(value) ]) )
    create_array_r2 % shape = shape(value)
  END FUNCTION create_array_r2
  !
  ! construction of parameterized function objects
  TYPE(pfunc_type) FUNCTION create_pfunc_type(fp, param)
    PROCEDURE(pfunc) :: fp
    CLASS(*), INTENT(in), OPTIONAL :: param
    create_pfunc_type%fp => fp
    IF ( present(param) ) THEN
       ALLOCATE(create_pfunc_type%param, source=param)
    END IF
  END FUNCTION create_pfunc_type
  TYPE(pfunc_type) FUNCTION create_pfunc_type_array(fp_array, param)
    PROCEDURE(pfunc_array) :: fp_array
    CLASS(*), INTENT(in), OPTIONAL :: param
    create_pfunc_type_array%fp_array => fp_array
    IF ( present(param) ) THEN
       ALLOCATE(create_pfunc_type_array%param, source=param)
    END IF
  END FUNCTION create_pfunc_type_array
  !
  ! specific type-bound procedures
  ! (these remain invisible to clients)
  REAL FUNCTION f_scalar(this, x)
    CLASS(pfunc_type), INTENT(in) :: this
    REAL, INTENT(in) :: x

    IF ( associated(this%fp) ) THEN
       f_scalar = this%fp(x, this%param)
    ELSE IF ( associated(this%fp_array) ) THEN
       ASSOCIATE ( f_array => this%fp_array([x], this%param) )
         f_scalar = f_array(1)
       END ASSOCIATE
    ELSE
       ERROR STOP 'pfunc_type callback: uninitialized object'
    END IF
  END FUNCTION f_scalar
  FUNCTION f_array(this, x) RESULT(r)
    CLASS(pfunc_type), INTENT(in) :: this
    REAL, INTENT(in) :: x(:)
    REAL :: r(size(x))

    ! Note that support for the scalar version is omitted here, since
    ! the procedure call overhead, including type resolution, would
    ! significantly impact performance.
    IF ( associated(this%fp_array) ) THEN
       r = this%fp_array(x, this%param)
    ELSE
       ERROR STOP 'pfunc_type callback: uninitialized object'
    END IF
  END FUNCTION f_array
END MODULE mod_utility_types
