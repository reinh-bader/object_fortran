SUBMODULE (mod_sortable) smod_constructor
CONTAINS
   MODULE PROCEDURE create_sortable
      USE mod_sortable_extensions, ONLY : sortable_string
      
      IF ( allocated(init%description) ) THEN
         SELECT CASE ( init%description )
         CASE ('sortable_string')
            IF ( allocated(init%value) ) THEN
               SELECT TYPE ( value => init%value(1) )
               TYPE IS (CHARACTER(len=*)) 
                  ALLOCATE( r, source=sortable_string(value) )
               END SELECT
            ELSE
               ALLOCATE( sortable_string :: r )
            END IF
         END SELECT
      END IF
   END PROCEDURE
   MODULE PROCEDURE create_sortable_array
      USE mod_sortable_extensions, ONLY : sortable_string
      
      INTEGER :: i
      IF ( size(init) < 1 ) RETURN
      IF ( allocated(init(1)%description) ) THEN
         SELECT CASE ( init(1)%description )
         CASE ('sortable_string')
            ALLOCATE (sortable_string :: r(size(init)) )
            DO i = 1, size(init)
               SELECT TYPE ( value => init(i)%value(1) )
               TYPE IS (CHARACTER(len=*)) 
                  SELECT TYPE ( element => r(i) )
                  TYPE IS (sortable_string)
                     element%string = trim(value)
                  END SELECT
               END SELECT
            END DO
         END SELECT
      END IF
   END PROCEDURE
   MODULE PROCEDURE create_sortable_from_string
      USE mod_sortable_extensions, ONLY : sortable_string
     
      SELECT CASE ( trim(type) )
      CASE ('sortable_string')
          ALLOCATE( r, source=sortable_string(trim(value)) )
      END SELECT
   END PROCEDURE
END SUBMODULE
