MODULE mod_pbody
   USE mod_utility_types, ONLY : change => any_object
   IMPLICIT none
   
   PRIVATE
   PUBLIC :: calculate_force_field, propagate, change, variant
   
   INTEGER, PARAMETER :: strlen = 10
   CHARACTER(len=strlen) :: variant = "direct"
  
   TYPE, PUBLIC :: pbody(np)
      INTEGER, LEN :: np
      REAL :: mass(np)
      REAL :: pos(np, 3), vel(np, 3)
   CONTAINS
      PROCEDURE :: update => update_pbody
   END TYPE

   TYPE, PUBLIC :: force_field(np)
      INTEGER, LEN :: np
      REAL :: f(np, 3)
   END type force_field
   
   TYPE, EXTENDS(change), PUBLIC :: propagate_change
      REAL, POINTER :: wk(:,:) ! will be np, 3
   END TYPE
   
   INTERFACE propagate_change
      MODULE PROCEDURE create_propagate_change
   END INTERFACE
   
CONTAINS
!
! type bound procedure follows
   SUBROUTINE update_pbody(a_pbody, a_change, mask)
      CLASS(pbody(np=*)), INTENT(inout) :: a_pbody
      CLASS(change), INTENT(in), TARGET :: a_change
      LOGICAL, OPTIONAL, INTENT(in) :: mask(:)
      REAL, POINTER :: pw(:,:)
      
      INTEGER :: i
   
      IF ( allocated(a_change%description) ) THEN
         component : SELECT CASE ( trim(a_change%description) )
         CASE ('mass')
            IF ( .NOT. allocated(a_change%value) ) EXIT component
            SELECT TYPE ( delta => a_change%value )
            TYPE IS (real)
              IF ( present(mask) ) THEN
                 WHERE (mask) a_pbody%mass(:) = a_pbody%mass(:) + delta
              ELSE 
                 a_pbody%mass(:) = a_pbody%mass(:) + delta
              END IF
           END SELECT
         CASE ('momentum')
            IF ( .NOT. allocated(a_change%value) ) EXIT component
            SELECT TYPE ( delta => a_change%value )
            TYPE IS (real)
               IF ( size(delta) >= 3 * a_pbody%np ) THEN
                  pw(1:a_pbody%np, 1:3) => delta
                  IF ( present(mask) ) THEN
                     DO i=1, a_pbody%np
                       IF (mask(i)) &
                       a_pbody % vel(i,:) = a_pbody % vel(i,:) + pw(i,:) / a_pbody % mass(i)
                     END DO 
                  ELSE
                     DO i=1, a_pbody%np
                        a_pbody % vel(i,:) = a_pbody % vel(i,:) + pw(i,:) / a_pbody % mass(i)
                     END DO 
                 
                  END IF 
               END IF
            END SELECT
         CASE ('position')
            IF ( .NOT. allocated(a_change%value) ) EXIT component
            SELECT TYPE ( delta => a_change%value )
            TYPE IS (real)
               IF ( size(delta) >= 3 * a_pbody%np ) THEN
                  pw(1:a_pbody%np, 1:3) => delta
                  IF ( present(mask) ) THEN
                     DO i = 1, a_pbody%np
                        IF (mask(i)) a_pbody%pos(i,:) = a_pbody%pos(i,:) + pw(i,:)
                     END DO
                  ELSE
                     a_pbody%pos = a_pbody%pos + pw
                  END IF
               END IF
            END SELECT
         END SELECT component
      END IF
   END SUBROUTINE
   
  TYPE(propagate_change) FUNCTION create_propagate_change(description, value, shape)
    CHARACTER(len=*), INTENT(in) :: description
    REAL, INTENT(in) :: value(:)
    INTEGER, INTENT(in) :: shape(2)
    TARGET :: create_propagate_change
    
    create_propagate_change%change = change(description, value, shape)
    SELECT TYPE ( p => create_propagate_change%value )
    TYPE IS (REAL)
       create_propagate_change%wk(1:shape(1),1:shape(2)) => p
    END SELECT
    WRITE(*,*) 'wk associated? ', associated(create_propagate_change%wk)
  END FUNCTION

!   
!  force field calculation (at bodies' position) 
   SUBROUTINE calculate_force_field(a_field, bodies)
      TYPE(force_field(np=*)), INTENT(inout) :: a_field
      TYPE(pbody(np=*)), INTENT(in) :: bodies
      REAL, PARAMETER :: g = 9.81          ! gravitative acceleration on surface of earth in m/s**2   
      REAL, PARAMETER :: rad_e = 6.371e+3  ! mean radius of earth in m
           
      a_field%f(:,1) = 0.0
      a_field%f(:,2) = 0.0
      a_field%f(:,3) = - g * bodies%mass(:) / (1.0 + bodies%pos(:,3)/rad_e)**2
      
      !write(*,'("force ",3ES10.3)')  a_field%f(:,1)

   END SUBROUTINE
   
!
!  propagating bodies across time, given the force field.   
   SUBROUTINE propagate(bodies, num_active, a_field, delta_t, wk_pos, wk_mom)
      TYPE(pbody(np=*)), INTENT(inout), TARGET :: bodies
      INTEGER, INTENT(out) :: num_active
      TYPE(force_field(np=*)), INTENT(in), TARGET :: a_field
      TYPE(propagate_change), INTENT(inout), TARGET :: wk_pos, wk_mom
      REAL :: delta_t
      INTEGER :: i
      
      SELECT CASE( trim(variant) )
      CASE ("direct") 

         ASSOCIATE ( pos => bodies%pos, vel => bodies%vel, mass => bodies%mass, f => a_field%f )
            num_active = 0
            DO i=1, bodies%np
               IF ( pos(i, 3) > 0.0 ) THEN
                  pos(i,:) = pos(i,:) + delta_t * vel(i,:)
                  vel(i,:) = vel(i,:) + delta_t * f(i,:) / mass(i)
                  num_active = num_active + 1
                  !
                  ! inelastic landing (implies that no further propagation is done)
                  IF ( pos(i,3) <= 0.0 ) THEN
                     pos(i,3) = 0.0
                     vel(i,:) = 0.0
                     num_active = num_active - 1
                  END IF
               END IF
            END DO
         END ASSOCIATE 
         
      CASE ("procedure")
      
         wk_pos%wk = delta_t * bodies%vel
         wk_mom%wk = delta_t * a_field%f
           
         num_active = 0
         ASSOCIATE ( mask => bodies%pos(:,3) > 0.0 )
            CALL bodies%update(wk_pos, mask)
            CALL bodies%update(wk_mom, mask)
         END ASSOCIATE
         ASSOCIATE ( mask => bodies%pos(:,3) <= 0.0 )
            WHERE ( mask )
               bodies%pos(:,3) = 0.0
               bodies%vel(:,1) = 0.0
               bodies%vel(:,2) = 0.0
               bodies%vel(:,3) = 0.0
            END WHERE
            num_active = bodies%np - count(mask)
         END ASSOCIATE 

      END SELECT
      !write(*,'("b1 mass, pos and vel: ",7ES10.3)')  bodies(1)

   END SUBROUTINE
END MODULE
PROGRAM performance_variants
   USE mod_pbody
   USE timer
   IMPLICIT none
   
   INTEGER :: i, it, iu, nbodies, num_active
   TYPE(pbody(np=:)), ALLOCATABLE :: bodies
   TYPE(force_field(np=:)), ALLOCATABLE :: grav
   TYPE(propagate_change), ALLOCATABLE, TARGET :: wk_pos, wk_mom
   DOUBLE PRECISION :: ti
   REAL :: delta_t, sim_t
   LOGICAL :: tinv
   
   WRITE(*,*) 'Enter the number of bodies.'
   READ(*,*) nbodies
   WRITE(*,*) 'Change from direct to TBP invocation (T/F)?'
   READ(*,*) tinv
   IF (tinv) variant = "procedure"
   
   WRITE(*,*) 'Simulating ',nbodies, ' bodies with variant "',variant,'"'
   
   ALLOCATE( pbody(np=nbodies) :: bodies )
   ALLOCATE( force_field(np=nbodies) :: grav )
   ALLOCATE( propagate_change :: wk_pos, wk_mom )

   CALL set_initial(bodies)
   wk_pos = propagate_change('position', [(0.0, i = 1, 3*nbodies)], [nbodies, 3])
   wk_mom = propagate_change('momentum', [(0.0, i = 1, 3*nbodies)], [nbodies, 3])
   
   
   num_active = nbodies
   delta_t = 1.0e-3
   sim_t = 0.0
   it = 0
   ti = wtime()
   simloop : DO 
      it = it + 1
      sim_t = sim_t + delta_t
      CALL calculate_force_field(grav, bodies)
      CALL propagate(bodies, num_active, grav, delta_t, wk_pos, wk_mom)
      
      IF ( mod(it, 1000) == 0 ) THEN
         WRITE(*,fmt='(I0, " active particles at simulation time ",F10.3)' ) num_active, sim_t
         WRITE(*,fmt='(" Height range: ",2ES13.6)') minval(bodies%pos(:,3)), maxval(bodies%pos(:,3))
      END IF
      
      ! complete simulation once all bodies have landed
      IF (num_active == 0) EXIT simloop
   END DO simloop
   ti = wtime() - ti
   
   WRITE(*,fmt='("Simulation took ",F7.2," seconds.")') ti
   WRITE(*,fmt='("Simulation-internal time: ",F7.2," seconds.")') sim_t
   
   OPEN(newunit=iu, file='finpos.dat', form='FORMATTED', action='WRITE', status='REPLACE')
   WRITE(iu, fmt='(*(2ES11.3,/))') ( bodies%pos(i,1:2), i=1, nbodies ) 
   CLOSE(iu)
   
   
CONTAINS
   SUBROUTINE set_initial(bodies)
      TYPE(pbody(np=*)), INTENT(inout) :: bodies
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
      
      CALL random_init(repeatable = .true., image_distinct = .false.)
      DO i=1, bodies%np
         CALL random_number(rand)
         
         bodies%pos(i,:) = [ xymin + rand(1)*(xymax-xymin), &
                             xymin + rand(2)*(xymax-xymin), &
                             zmin +  rand(3)*(zmax-zmin) ]
         bodies%vel(i,:) = [ vxymin + rand(4)*(vxymax-vxymin), &
                             vxymin + rand(5)*(vxymax-vxymin), &
                             vzmin +  rand(6)*(vzmax-vzmin) ]
                           
         bodies%mass(i) =    mmin + rand(7)*(mmax - mmin)                
      END DO
  
   END SUBROUTINE
END PROGRAM
