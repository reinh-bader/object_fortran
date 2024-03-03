MODULE mod_wtype
   USE mod_utility_types, ONLY : init_comp => any_object

   TYPE :: wtype
      INTEGER :: nonzeros = -1
      REAL, ALLOCATABLE :: w(:,:)
   END TYPE wtype
CONTAINS
   SUBROUTINE setup_wtype(a_wtype, a_comp)
      TYPE(wtype), INTENT(inout) :: a_wtype
      TYPE(init_comp), INTENT(in), TARGET :: a_comp
      
      INTEGER :: wsize
      REAL, POINTER :: pw(:,:)
      
      SELECT CASE (a_comp%description)
      CASE ("nonzeros")
         IF ( allocated(a_comp%value) ) THEN
            SELECT TYPE ( nonzeros => a_comp%value(1) )
            TYPE IS (INTEGER)
               a_wtype%nonzeros = nonzeros
            END SELECT
         END IF
      CASE ("w")
         IF ( allocated(a_comp%value) .AND. allocated(a_comp%shape) ) THEN
            wsize = size(a_comp%value)
            IF ( wsize >= product(a_comp%shape) ) THEN
               SELECT TYPE ( w => a_comp%value )
               TYPE IS (REAL)
                  pw(1:a_comp%shape(1), 1:a_comp%shape(2)) => w
                  a_wtype%w = pw 
               END SELECT
            END IF
         END IF
      END SELECT
   END SUBROUTINE setup_wtype
END MODULE mod_wtype
PROGRAM object_setup
   USE mod_wtype
   TYPE(init_comp) :: c_nz, c_w
   TYPE(wtype) :: my_wtype
   INTEGER :: i, j

   c_nz = init_comp("nonzeros", 28)
   c_w = init_comp("w", [ ((real (max(0, min(i-j+2, j-i+2))), j=1, 10), i=1, 10) ], [10, 10] )

   CALL setup_wtype(my_wtype, c_nz) 
   CALL setup_wtype(my_wtype, c_w)  

   WRITE(*, fmt='(''Nonzeros:'',i0)') my_wtype%nonzeros
   WRITE(*, fmt='(''w:'')')
   DO i=1, 10
      WRITE(*, fmt='(10f6.1)') my_wtype%w(i,:)
   END DO
END PROGRAM



