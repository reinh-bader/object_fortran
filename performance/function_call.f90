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
  USE timer
  IMPLICIT none

  TYPE(pfunc_type) :: pfunc_obj
  REAL, PARAMETER :: piby4 = atan(1.0), piby4_arr(4) = [ piby4, 2.*piby4, 3*piby4, 4*piby4 ]
  INTEGER :: ndim, i
  REAL, ALLOCATABLE :: array(:)
  DOUBLE PRECISION t1, t2, s1, s2

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
  
  WRITE(*,*) 'Enter array size for performance evaluation.'
  READ(*,*) ndim
  ALLOCATE( array(ndim) ) 
  DO i=1,ndim
     array(i) = 0.1 + 0.1*real(i-1)/real(ndim-1)
  END DO

  pfunc_obj = pfunc_type(psin, 2.5)
  t1 = wtime()
  DO i=1, ndim
     array(i) = pfunc_obj%f(array(i))
  END DO
  t1 = wtime() - t1
  s1 = sum(array)
  WRITE(*,fmt='("Sum for scalar version: ",ES13.6)') s1
  WRITE(*,fmt='("Time (s) for scalar version: ",F10.6)') t1
  
  DO i=1,ndim
     array(i) = 0.1 + 0.1*real(i-1)/real(ndim-1)
  END DO
  pfunc_obj = pfunc_type(psin_array, 2.5)
  t2 = wtime()
  array = pfunc_obj%f(array) 
  t2 = wtime() - t2
  s2 = sum(array)
  WRITE(*,fmt='("Sum for array version: ",ES13.6)') s2
  WRITE(*,fmt='("Time (s) for array version: ",F10.6)') t2
    
END PROGRAM function_call
