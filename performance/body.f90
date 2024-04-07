MODULE mod_body
   USE mod_utility_types, ONLY : change => any_object
   IMPLICIT none
   
   PRIVATE
   PUBLIC :: calculate_force_field, propagate, change, variant
   
   INTEGER, PARAMETER :: strlen = 10
   CHARACTER(len=strlen) :: variant = "direct"
  
   TYPE, PUBLIC :: body
      REAL :: mass
      REAL :: pos(3), vel(3)
   CONTAINS
      PROCEDURE :: update => update_body
   END TYPE

   TYPE, PUBLIC :: force_field
      REAL, ALLOCATABLE :: f(:,:)
   END type force_field
   
CONTAINS
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
! type bound procedure follows
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

!   
!  force field calculation (at bodies' position) 
   SUBROUTINE calculate_force_field(a_field, bodies)
      TYPE(force_field), INTENT(inout) :: a_field
      TYPE(body), INTENT(in) :: bodies(:)
      REAL, PARAMETER :: g = 9.81          ! gravitative acceleration on surface of earth in m/s**2   
      REAL, PARAMETER :: rad_e = 6.371e+3  ! mean radius of earth in m
      INTEGER :: i
      
      IF ( .not. allocated(a_field%f) ) &
		ERROR STOP 'calculate_force_field: a_field not set up'
      IF ( size(a_field%f, 2) < size(bodies) .OR. size(a_field%f, 1) < 3 ) &
        ERROR STOP 'calculate_force_field: a_field%f has insufficient size'
        
      DO i=1, size(bodies) 
         a_field%f(1,i) = 0.0
         a_field%f(2,i) = 0.0
         a_field%f(3,i) = - g * bodies(i)%mass / (1.0 + bodies(i)%pos(3)/rad_e)**2
      END DO  
      
      !write(*,'("force ",3ES10.3)')  a_field%f(:,1)

   END SUBROUTINE
   
!
!  propagating bodies across time, given the force field.   
   SUBROUTINE propagate(bodies, num_active, a_field, delta_t)
      TYPE(body), INTENT(inout) :: bodies(:)
      INTEGER, INTENT(out) :: num_active
      TYPE(force_field), INTENT(in) :: a_field
      REAL :: delta_t
      INTEGER :: i
      
      IF ( .not. allocated(a_field%f) ) &
		ERROR STOP 'propagate: a_field not set up'
      IF ( size(a_field%f, 2) < size(bodies) .OR. size(a_field%f, 1) < 3 ) &
        ERROR STOP 'propagate: a_field%f has insufficient size'


      SELECT CASE( trim(variant) )
      CASE ("direct") 

         num_active = 0
         DO i=1, size(bodies)
            IF ( bodies(i)%pos(3) > 0.0 ) THEN
               bodies(i)%pos = bodies(i)%pos + delta_t * bodies(i)%vel
               bodies(i)%vel = bodies(i)%vel + delta_t * a_field%f(1:3,i) / bodies(i)%mass
               num_active = num_active + 1
               !
               ! inelastic landing (implies no future propagation)
               IF ( bodies(i)%pos(3) <= 0.0 ) THEN
                  bodies(i)%pos(3) = 0.0
                  bodies(i)%vel = 0.0
                  num_active = num_active - 1
               END IF
            END IF
         END DO
         
      CASE ("procedure")
      
         num_active = 0
         DO i=1, size(bodies)
            IF ( bodies(i)%pos(3) > 0.0 ) THEN
               CALL bodies(i)%update(change('position', delta_t * bodies(i)%vel))
               CALL bodies(i)%update(change('momentum', delta_t * a_field%f(1:3,i)))
               num_active = num_active + 1
               !
               ! inelastic landing (implies no future propagation)
               IF ( bodies(i)%pos(3) <= 0.0 ) THEN
                  bodies(i)%pos(3) = 0.0
                  bodies(i)%vel = 0.0
                  num_active = num_active - 1
               END IF
            END IF
         END DO
      END SELECT
      !write(*,'("b1 mass, pos and vel: ",7ES10.3)')  bodies(1)

   END SUBROUTINE
END MODULE
PROGRAM performance_variants
   USE mod_body
   USE timer
   IMPLICIT none
   
   INTEGER :: i, it, iu, nbodies, num_active
   TYPE(body), ALLOCATABLE :: bodies(:)
   TYPE(force_field) :: grav
   DOUBLE PRECISION :: ti
   REAL :: delta_t, sim_t
   LOGICAL :: tinv
   
   WRITE(*,*) 'Enter the number of bodies.'
   READ(*,*) nbodies
   WRITE(*,*) 'Change from direct to TBP invocation (T/F)?'
   READ(*,*) tinv
   IF (tinv) variant = "procedure"
   
   WRITE(*,*) 'Simulating ',nbodies, ' bodies with variant "',variant,'"'
   
   ALLOCATE( bodies(nbodies), grav%f(3, nbodies) )
   CALL set_initial(bodies)
   
   num_active = nbodies
   delta_t = 1.0e-3
   sim_t = 0.0
   it = 0
   ti = wtime()
   simloop : DO 
      it = it + 1
      sim_t = sim_t + delta_t
      CALL calculate_force_field(grav, bodies)
      CALL propagate(bodies, num_active, grav, delta_t)
      
      IF ( mod(it, 1000) == 0 ) THEN
         WRITE(*,fmt='(I0, " active particles at simulation time ",F10.3)' ) num_active, sim_t
         WRITE(*,fmt='(" Height range: ",2ES13.6)') minval(bodies%pos(3)), maxval(bodies%pos(3))
      END IF
      
      ! complete simulation once all bodies have landed
      IF (num_active == 0) EXIT simloop
   END DO simloop
   ti = wtime() - ti
   
   WRITE(*,fmt='("Simulation took ",F7.2," seconds.")') ti
   WRITE(*,fmt='("Simulation-internal time: ",F7.2," seconds.")') sim_t
   
   OPEN(newunit=iu, file='finpos.dat', form='FORMATTED', action='WRITE', status='REPLACE')
   WRITE(iu, fmt='(*(2ES11.3,/))') ( bodies(i)%pos(1:2), i=1, nbodies ) 
   CLOSE(iu)
   
   
CONTAINS
   SUBROUTINE set_initial(bodies)
      TYPE(body), INTENT(inout) :: bodies(:)
      !
      ! position intervals (m)
      REAL, PARAMETER :: xymin = -100.
      REAL, PARAMETER :: xymax =  100.
      REAL, PARAMETER :: zmin = 100.
      REAL, PARAMETER :: zmax = 1000.
      !
      ! velocity intervals (m/s)
      REAL, PARAMETER :: vxymin = -10.
      REAL, PARAMETER :: vxymax = -10.
      REAL, PARAMETER :: vzmin  = -2. 
      REAL, PARAMETER :: vzmax  = 42. 
      !
      ! mass intervals
      REAL, PARAMETER :: mmin = 1.0
      REAL, PARAMETER :: mmax = 2.0
      
      INTEGER :: i
      REAL :: rand(7)
      
      DO i=1, size(bodies)
         CALL random_number(rand)
         
         bodies(i)%pos = [ xymin + rand(1)*(xymax-xymin), &
                           xymin + rand(2)*(xymax-xymin), &
                           zmin +  rand(3)*(zmax-zmin) ]
         bodies(i)%vel = [ vxymin + rand(4)*(vxymax-vxymin), &
                           vxymin + rand(5)*(vxymax-vxymin), &
                           vzmin +  rand(6)*(vzmax-vzmin) ]
                           
         bodies(i)%mass =  mmin + rand(7)*(mmax - mmin)                
      END DO
  
   END SUBROUTINE
END PROGRAM
