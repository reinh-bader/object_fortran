SUBMODULE (mod_sortable) smod_constructor
CONTAINS
   MODULE PROCEDURE create_sortable
      USE mod_sortable_extensions, ONLY : sortable_string
      
      IF ( allocated(init%description) .AND. allocated(init%value) ) THEN
         SELECT CASE (init%description)
         CASE ('sortable_string')
            SELECT TYPE ( value => init%value(1) )
            TYPE IS (CHARACTER(len=*)) 
               ALLOCATE( r, source=sortable_string(value) )
            END SELECT
         END SELECT
      END IF
   END PROCEDURE
END SUBMODULE
