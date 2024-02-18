MODULE mod_polynomial
   IMPLICIT none
   PRIVATE
   PUBLIC :: eval_polynomial, OPERATOR (*), WRITE(formatted)
   TYPE, PUBLIC :: polynomial
      PRIVATE              ! prevent explicit component allocation with
                           ! potentially incorrect bounds outside the module 
      REAL, ALLOCATABLE :: a(:)
   END TYPE
   
   INTERFACE polynomial    ! overload to assure correct lower bound
                           ! when creating a polynomial object
      MODULE PROCEDURE :: create_polynomial
   END INTERFACE
   INTERFACE OPERATOR (*)
      MODULE PROCEDURE multiply_polynomial
   END INTERFACE
   INTERFACE WRITE(formatted)
      MODULE PROCEDURE write_fmt_polynomial
   END INTERFACE
CONTAINS
   ELEMENTAL REAL FUNCTION eval_polynomial(a_polynomial, x)
      TYPE(polynomial), INTENT(in) :: a_polynomial
      REAL, INTENT(in) :: x
     
      INTEGER :: i
     
      eval_polynomial = 0.0
      IF ( allocated(a_polynomial%a) ) THEN
         ASSOCIATE(a => a_polynomial%a, imax => ubound(a_polynomial%a,1) )
            eval_polynomial = a(imax)
            DO i = imax - 1, 0, -1 
               eval_polynomial = x * eval_polynomial + a(i)
            END DO
         END ASSOCIATE  
      END IF
   END FUNCTION
   PURE TYPE(polynomial) FUNCTION create_polynomial(a)
      REAL, INTENT(in) :: a(0:)  ! upper bound is interpreted as degree
      create_polynomial%a = a    ! preserves remapped bounds of right hand side
   END FUNCTION
   PURE TYPE(polynomial) FUNCTION multiply_polynomial(p1, p2)
      TYPE(polynomial), INTENT(in) :: p1, p2
      
      INTEGER :: j, l, lmax
      
      lmax = ubound(p1%a,1) + ubound(p2%a,1) 
      ALLOCATE( multiply_polynomial%a(0:lmax) )
      ASSOCIATE( a => p1%a, b => p2%a, c => multiply_polynomial%a, &
                 jmax => ubound(p1%a,1), kmax => ubound(p2%a,1) ) ! association list
         DO l = 0, lmax
            c(l) = 0
            DO j = max(0, l-kmax), min(jmax, l)
               c(l) = c(l) + a(j) * b(l-j)
            END DO
         END DO
      END ASSOCIATE
   END FUNCTION
   SUBROUTINE write_fmt_polynomial(dtv, unit, iotype, v_list, iostat, iomsg)
      CLASS(polynomial), INTENT(in) :: dtv
      INTEGER, INTENT(in) :: unit, v_list(:)
      CHARACTER(len=*), INTENT(in) :: iotype
      INTEGER, INTENT(out) :: iostat
      CHARACTER(len=*), INTENT(inout) :: iomsg
      
      SELECT CASE (iotype)
      CASE ('LISTDIRECTED')
         WRITE(unit, fmt=*, iostat=iostat, iomsg=iomsg) &
			"polynomial of degree ", ubound(dtv%a,1), ":", dtv%a
      CASE default
         iostat = -1
         iomsg = 'iotype ' // TRIM(iotype) // ' not implemented'
         RETURN
      END SELECT
   END SUBROUTINE
END MODULE
PROGRAM exercise_polynomial
  USE mod_polynomial
  IMPLICIT none
  
  TYPE(polynomial) :: p, q
  REAL :: x(2), y(2)
!
! overloaded constructor
!
  p = polynomial( [ 2., 3., 1. ] )
  x = [ 0., 2. ]
  y = eval_polynomial(p, x)
  WRITE(*,'("p( ",F5.1,",",F5.1," ) = ",F5.1,",",F5.1)') x, y
!
! default assignment
!
  p = polynomial( a = [ -1., 4., 0., 1. ] ) ! use keyword, just for fun
  q = p
  x = [ 0., 2. ]
  y = eval_polynomial(q, x)
  WRITE(*,'("q( ",F5.1,",",F5.1," ) = ",F5.1,",",F5.1)') x, y
!
! multiply and write coefficients
!
  p = polynomial( [ 2., 3., 1. ] ) 
  q = polynomial( [ 4., -3. ] ) 
  p = p * q                                 ! invokes overloaded *
  y = eval_polynomial(p, x)
  WRITE(*,fmt=*) p                          ! invokes UDDTIO procedure
  WRITE(*,'("p( ",F5.1,",",F5.1," ) = ",F5.1,",",F5.1)') x, y
END PROGRAM
