MODULE mod_wtype
   USE mod_utility_types, ONLY : initialize => any_object

   TYPE :: wtype
      PRIVATE
      INTEGER :: nonzeros = -1
      REAL, ALLOCATABLE :: w(:,:)
   END TYPE wtype
CONTAINS
   SUBROUTINE setup_wtype(a_wtype, a_component)
      ! in-place setting to avoid memory bursts for large objects
      TYPE(wtype), INTENT(inout) :: a_wtype
      TYPE(initialize), INTENT(in), TARGET :: a_component
      
      INTEGER :: wsize
      REAL, POINTER :: pw(:,:)
      
      SELECT CASE (a_component%description)
      CASE ("nonzeros")
         IF ( allocated(a_component%value) ) THEN
            SELECT TYPE ( nonzeros => a_component%value(1) )
            TYPE IS (INTEGER)
               a_wtype%nonzeros = nonzeros
            END SELECT
         END IF
      CASE ("w")
         IF ( allocated(a_component%value) .AND. allocated(a_component%shape) ) THEN
            wsize = size(a_component%value)
            IF ( wsize >= product(a_component%shape) ) THEN
               SELECT TYPE ( w => a_component%value )
               TYPE IS (REAL)
                  pw(1:a_component%shape(1), 1:a_component%shape(2)) => w
                  a_wtype%w = pw 
               END SELECT
            END IF
         END IF
      END SELECT
   END SUBROUTINE setup_wtype
   SUBROUTINE print_wtype(a_wtype)
      TYPE(wtype), INTENT(in) :: a_wtype
      INTEGER :: i
      WRITE(*, fmt='(''Value of component nonzeros:'',i0)') a_wtype%nonzeros
      WRITE(*, fmt='(''Value of component w:'')')
      DO i=1, size(a_wtype%w, 1)
         WRITE(*, fmt='(*(f6.1))') a_wtype%w (i,:)
      END DO
   END SUBROUTINE
END MODULE mod_wtype
PROGRAM object_setup
   USE mod_wtype
   TYPE(initialize) :: c_nz, c_w
   TYPE(wtype) :: my_wtype
   INTEGER :: i, j
   INTEGER :: ndim 

   ndim = 10
   
   ASSOCIATE ( my_data => [ ((real (max(0, min(i-j+2, j-i+2))), j=1, ndim), i=1, ndim) ] )
      c_nz = initialize("nonzeros", count(my_data /= 0))
      c_w = initialize("w", my_data, [ ndim, ndim ] )
   END ASSOCIATE

   CALL setup_wtype(my_wtype, c_nz) 
   CALL setup_wtype(my_wtype, c_w)  

   CALL print_wtype(my_wtype)
END PROGRAM



