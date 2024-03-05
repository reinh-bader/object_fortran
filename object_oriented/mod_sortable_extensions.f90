MODULE mod_sortable_extensions
   USE mod_sortable
   USE, INTRINSIC :: iso_fortran_env, ONLY : error_unit
   IMPLICIT none
   
   PRIVATE 
   
   TYPE, PUBLIC, EXTENDS(sortable) :: sortable_string
      CHARACTER(len=:), ALLOCATABLE :: string
   CONTAINS
      PROCEDURE :: less_than => less_than_string
      PROCEDURE, NOPASS :: type_of => type_of_string
      PROCEDURE :: value_of => value_of_string
      PROCEDURE :: set => set_string
      PROCEDURE :: write_fmt => write_fmt_string
   END TYPE
CONTAINS
   PURE LOGICAL FUNCTION less_than_string(s1, s2)
      CLASS(sortable_string), INTENT(in) :: s1
      CLASS(sortable), INTENT(in) :: s2
      
      SELECT TYPE (s2)
      CLASS IS (sortable_string)
         IF ( allocated(s1%string) .AND. allocated(s2%string) ) THEN
            less_than_string = llt(s1%string,s2%string)
         ELSE
            less_than_string = .false.    
         END IF
      CLASS default
         less_than_string = .false.
      END SELECT
   END FUNCTION   
   PURE FUNCTION type_of_string() RESULT (tstr)
      CHARACTER(len=:), ALLOCATABLE :: tstr 
      tstr = 'sortable_string'
   END FUNCTION
   PURE FUNCTION  value_of_string(s) RESULT (tstr)
      CLASS(sortable_string), INTENT(in) :: s
      CHARACTER(len=:), ALLOCATABLE :: tstr 
      tstr = trim(s%string)
   END FUNCTION
   SUBROUTINE set_string(s, init)
      CLASS(sortable_string), INTENT(inout) :: s
      TYPE(initialize), INTENT(in) :: init
      
      IF ( allocated(init%value) ) THEN
         SELECT TYPE ( value => init%value(1)) 
         TYPE IS ( CHARACTER(len=*) ) 
            s%string = value
         END SELECT
      END IF
   END SUBROUTINE
   SUBROUTINE write_fmt_string(dtv, unit, iotype, v_list, iostat, iomsg)
      CLASS(sortable_string), INTENT(in) :: dtv
      INTEGER, INTENT(in) :: unit, v_list(:)
      CHARACTER(len=*), INTENT(in) :: iotype
      INTEGER, INTENT(out) :: iostat
      CHARACTER(len=*), INTENT(inout) :: iomsg
      
!      WRITE(error_unit,*) 'iotype in write_fmtstring ', trim(iotype)
      
      SELECT CASE ( trim(iotype) )
      CASE ('LISTDIRECTED')
!         WRITE(error_unit,*) ' to LISTDIRECTED'
         WRITE(unit, fmt=*, iostat=iostat, iomsg=iomsg) '"',dtv%string,'"'
!         WRITE(error_unit,*) ' LISTDIRECTED done.'
      CASE ('NAMELIST')
!         WRITE(error_unit,*) ' to NAMELIST'
         WRITE(unit, fmt=*, iostat=iostat, iomsg=iomsg) '"',dtv%string,'"'
!         WRITE(error_unit,*) ' NAMELIST done.'
      CASE default
         iostat = 129
         iomsg = 'iotype ' // trim(iotype) // ' not implemented'
      END SELECT
   END SUBROUTINE
END MODULE
