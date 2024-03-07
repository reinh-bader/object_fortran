MODULE mod_body
   USE mod_utility_types, ONLY : change => any_object
   IMPLICIT none
   
   PRIVATE
   PUBLIC :: legacy_invocations, change, body_fmt
   
   INTEGER, PARAMETER :: format_strlen = 105
   CHARACTER(len=format_strlen), PARAMETER :: body_fmt = &
             '("mass=",ES9.2,", pos=",3F5.2,", vel=",3F5.2,:,", charge=",ES9.2)'
   
   TYPE, PUBLIC :: body
      REAL :: mass
      REAL :: pos(3), vel(3)
   CONTAINS
      PROCEDURE :: update => update_body
   END TYPE
   
   TYPE, EXTENDS(body), PUBLIC :: charged_body
      REAL :: charge
   CONTAINS
      PROCEDURE :: update => update_charged_body
   END TYPE
CONTAINS
!  this procedure serves to exercise the legacy features 
!  (which currently are not accessible outside the module)
   SUBROUTINE legacy_invocations()
     TYPE(body) :: my_basketball = body(1.5, [0.0, 0.0, 2.0], [10.0, 0.0, 0.0])
     
     CALL kick(my_basketball, dp=[-3.0, 0.0, 4.5])
     CALL accrete(my_basketball, dm=-0.05)         ! lose some air
     WRITE(*,*) 'my_basketball has value '
     WRITE(*,fmt=body_fmt) my_basketball
   END SUBROUTINE
!
! regular module procedures follow
   PURE SUBROUTINE kick(a_body, dp)
      TYPE(body), INTENT(inout) :: a_body
      REAL, intent(in) :: dp(3)

      a_body % vel(:) = a_body % vel(:) + dp(:) / a_body % mass  
   END SUBROUTINE 
   PURE SUBROUTINE accrete(a_body, dm)
      TYPE(body), INTENT(inout) :: a_body
      REAL, intent(in) :: dm
    
      a_body%mass = a_body%mass + dm
   END SUBROUTINE accrete
!   
! type bound procedures follow
   SUBROUTINE update_body(a_body, a_change)
      CLASS(body), INTENT(inout) :: a_body
      TYPE(change), INTENT(in) :: a_change
   
      IF ( allocated(a_change%description) .AND. allocated(a_change%value) ) THEN
         SELECT CASE ( trim(a_change%description) )
         CASE ('mass')
            SELECT TYPE ( delta => a_change%value(1) )
            TYPE IS (real)
               CALL accrete(a_body, delta) 
            END SELECT
         CASE ('momentum')
            SELECT TYPE ( delta => a_change%value )
            TYPE IS (real)
               IF ( size(delta) >= 3 ) CALL kick(a_body, delta(1:3))
            END SELECT
         CASE ('position')
            SELECT TYPE ( delta => a_change%value )
            TYPE IS (real)
               IF ( size(delta) >= 3) a_body%pos = a_body%pos + delta(1:3) 
            END SELECT
         END SELECT
      END IF
   END SUBROUTINE
   SUBROUTINE update_charged_body(a_body, a_change)
      CLASS(charged_body), INTENT(inout) :: a_body
      TYPE(change), INTENT(in) :: a_change
   
      IF ( allocated(a_change%description) .AND. allocated(a_change%value) ) THEN
         SELECT CASE ( trim(a_change%description) )
         CASE ('charge')
            SELECT TYPE (delta => a_change%value(1))
            TYPE IS (real)
               a_body%charge = a_body%charge + delta
            END SELECT
         CASE default
            CALL a_body%body%update(a_change) ! assure that a change to a parent component is dealt with
         END SELECT
      END IF
   END SUBROUTINE
END MODULE
PROGRAM exercise_body
   USE mod_body
   IMPLICIT none
 
   CALL legacy_invocations()

   constr : BLOCK   
      TYPE(body) :: a_mutilated_proton
      TYPE(charged_body) :: a_proton
! Construct a_proton
      a_proton = charged_body(mass=1.672E-27, pos=[0.0, 0.0, 0.0], vel=[0.0, 0.0, 0.0], &
                              charge=1.602E-19)
      WRITE(*,*) 'a_proton first construction: '
      WRITE(*,fmt=body_fmt) a_proton

! Alternative construction with the same result
      a_mutilated_proton = body(mass=1.672E-27, pos=[0.0, 0.0, 0.0], vel=[0.0, 0.0, 0.0])
      a_proton = charged_body(body=a_mutilated_proton, charge=1.602E-19)
      WRITE(*,*) 'a_proton second construction: '
      WRITE(*,fmt=body_fmt) a_proton
   END BLOCK constr
   
   tbp_calls : BLOCK
      TYPE(change) ::  dc, dp
      CLASS(body), ALLOCATABLE :: my_polymorphic_body
      
! the following statement auto-allocates the left hand side
      my_polymorphic_body = charged_body(mass=1.5, pos=[0.,0.,0.], vel=[2.,0.,0.], charge=2.41E-5)
      dc = change(description='charge', value=[5.0E-6])
      dp = change(description='momentum', value=[-1.5,3.0,0.0])

! both the following dispatch to update_charged_body
      CALL my_polymorphic_body % update(dc) 
      CALL my_polymorphic_body % update(dp)
      
      
      SELECT TYPE (my_polymorphic_body) 
! here the programmer knows what the type is, so can keep things simple ...
      TYPE IS (charged_body)
         WRITE(*,*) 'my_polymorphic_body has value '
         WRITE(*,fmt=body_fmt) my_polymorphic_body
      END SELECT  
   END BLOCK tbp_calls
END PROGRAM
