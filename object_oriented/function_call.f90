MODULE mod_functions
  IMPLICIT none
CONTAINS
  PURE REAL FUNCTION psin(x, param)
    REAL, INTENT(in) :: x
    CLASS(*), INTENT(in), OPTIONAL :: param
    REAL :: factor
    factor = 1.
    IF ( present(param) ) THEN
       SELECT TYPE ( param )
       TYPE IS (REAL)
          factor = param
       TYPE IS (INTEGER)
          factor = real(param)
       END SELECT
    END IF
    psin = sin(factor*x)
  END FUNCTION psin
  PURE FUNCTION psin_array(x, param) RESULT(r)
    REAL, INTENT(in) :: x(:)
    REAL :: r(size(x))
    CLASS(*), INTENT(in), OPTIONAL :: param
    REAL :: factor
    factor = 1.
    IF ( present(param) ) THEN
       SELECT TYPE ( param )
       TYPE IS (REAL)
          factor = param
       TYPE IS (INTEGER)
          factor = real(param)
       END SELECT
    END IF
    r = sin(factor*x)
  END FUNCTION psin_array
END MODULE mod_functions
PROGRAM function_call
  USE mod_utility_types
  USE mod_functions
  IMPLICIT none

  TYPE(pfunc_type) :: pfunc_obj
  REAL, PARAMETER :: piby4 = atan(1.0), piby4_arr(4) = [ piby4, 2.*piby4, 3*piby4, 4*piby4 ]

  TYPE :: new
  END TYPE new

  TYPE(new) :: o_new

  pfunc_obj = pfunc_type(psin, 2)
  WRITE(*,*) pfunc_obj%f(piby4)

  pfunc_obj = pfunc_type(psin, 2.)
  WRITE(*,*) pfunc_obj%f(piby4)
  
  pfunc_obj = pfunc_type(psin)
  WRITE(*,*) pfunc_obj%f(piby4)
  
  pfunc_obj = pfunc_type(psin, o_new)
  WRITE(*,*) pfunc_obj%f(piby4)

  pfunc_obj = pfunc_type(psin_array, 2.)
  WRITE(*,*) pfunc_obj%f(piby4_arr)

  WRITE(*,*) pfunc_obj%f([piby4])
  
  WRITE(*,*) pfunc_obj%f(piby4)
  
  
    
END PROGRAM function_call
