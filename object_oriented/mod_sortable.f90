MODULE mod_sortable
   USE mod_utility_types, initialize => any_object
   IMPLICIT none
    
   TYPE, ABSTRACT :: sortable
   CONTAINS
      PROCEDURE(compare), DEFERRED :: less_than
      PROCEDURE(type_fun), DEFERRED, NOPASS :: type_of
      ! the following two are kludges to work around compiler bugs
      PROCEDURE(val_fun), DEFERRED :: value_of
      PROCEDURE(set_comp), DEFERRED :: set
      PROCEDURE(dtio_write_fmt), DEFERRED :: write_fmt
      GENERIC :: OPERATOR(<) => less_than
      GENERIC :: WRITE (FORMATTED) => write_fmt
   END TYPE
   
   ABSTRACT INTERFACE
      PURE LOGICAL FUNCTION compare(s1, s2)
         IMPORT :: sortable
         CLASS(sortable), INTENT(in) :: s1, s2 ! dispatch is via the first argument
      END FUNCTION
      PURE FUNCTION type_fun() RESULT (tstr)
         CHARACTER(len=:), ALLOCATABLE :: tstr
      END FUNCTION
      PURE FUNCTION val_fun(s) RESULT (tstr)
         IMPORT :: sortable
         CLASS(sortable), INTENT(in) :: s
         CHARACTER(len=:), ALLOCATABLE :: tstr
      END FUNCTION
      SUBROUTINE set_comp(s, init)
         IMPORT :: sortable, initialize
         CLASS(sortable), INTENT(inout) :: s
         TYPE(initialize), INTENT(in) :: init
      END SUBROUTINE
      SUBROUTINE dtio_write_fmt(dtv, unit, iotype, v_list, iostat, iomsg)
         IMPORT :: sortable
         CLASS(sortable), INTENT(in) :: dtv
         INTEGER, INTENT(in) :: unit, v_list(:)
         CHARACTER(len=*), INTENT(in) :: iotype
         INTEGER, INTENT(out) :: iostat
         CHARACTER(len=*), INTENT(inout) :: iomsg
      END SUBROUTINE
   END INTERFACE
   
   INTERFACE sortable
      PROCEDURE :: create_sortable
      PROCEDURE :: create_sortable_array
      PROCEDURE :: create_sortable_from_string
   END INTERFACE
   INTERFACE
      MODULE FUNCTION create_sortable(init) RESULT(r)
        CLASS(sortable), ALLOCATABLE :: r
        TYPE(initialize), INTENT(in) :: init
      END FUNCTION
      MODULE FUNCTION create_sortable_array(init) RESULT(r)
        CLASS(sortable), ALLOCATABLE :: r(:)
        TYPE(initialize), INTENT(in) :: init(:)
      END FUNCTION
      MODULE FUNCTION create_sortable_from_string(type, value) RESULT(r)
        CLASS(sortable), ALLOCATABLE :: r
        CHARACTER(len=*), INTENT(in) :: type, value
      END FUNCTION
   END INTERFACE
END MODULE
