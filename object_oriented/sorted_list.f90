MODULE mod_sorted_list
   USE mod_sortable
   USE, INTRINSIC :: iso_fortran_env, ONLY : error_unit
   IMPLICIT none
   PRIVATE
   PUBLIC :: WRITE(formatted), READ(formatted), OPERATOR(//), ASSIGNMENT(=), &
             add_to_sorted_list, lists_are_aliased
   TYPE, PUBLIC :: sorted_list
      PRIVATE
      CLASS(sortable), ALLOCATABLE :: data
      TYPE(sorted_list), POINTER :: next => null()
   CONTAINS
      FINAL :: delete_sorted_list
   END TYPE
   
   INTERFACE sorted_list 
! the default constructor is unavailable because the type is opaque;
! the specific has a different signature than the structure constructor
      MODULE PROCEDURE :: create_sorted_list
   END INTERFACE
   INTERFACE WRITE(formatted)
      MODULE PROCEDURE write_fmt_list
   END INTERFACE
   INTERFACE READ(formatted)
      MODULE PROCEDURE read_fmt_list
   END INTERFACE
   INTERFACE OPERATOR (//)
      MODULE PROCEDURE join_lists
   END INTERFACE
   INTERFACE ASSIGNMENT(=)
      MODULE PROCEDURE assign_sorted_list
   END INTERFACE
CONTAINS
   FUNCTION create_sorted_list(item_array) RESULT(head)
      CLASS(sortable), INTENT(in) :: item_array(:)
      TYPE(sorted_list) :: head
      
      INTEGER :: i
      
      IF ( size(item_array) == 0 ) RETURN
      
      DO i = 1, size(item_array)
         CALL add_to_sorted_list(head, item_array(i))
         ! handles tedious details of pointer fiddling
     END DO
   END FUNCTION
   SUBROUTINE add_to_sorted_list(list, item)
      TYPE(sorted_list), INTENT(inout), TARGET :: list
      CLASS(sortable), INTENT(in) :: item
     
      TYPE(sorted_list), POINTER :: head, current, prev, new
   
      ! incoming list is virginal
      IF ( .NOT. allocated(list%data) ) THEN
          ALLOCATE( list%data, source=item )
          RETURN
      END IF   
   
      head => list
      current => head
      prev => null()
      search : DO
         IF ( associated(current) ) THEN
            !WRITE(*,*) 'same (item, current) ? ', same_type_as(item, current%data)
            IF ( same_type_as(item, current%data) ) THEN
               IF ( item < current%data ) THEN ! insert before current
	   	          ALLOCATE( new )
		          IF ( associated(prev) ) THEN
       		         ALLOCATE( new%data, source=item )
                     new%next => current
		             prev%next => new
		          ELSE                        ! adjust beginning of list
		             ALLOCATE( new%data, source=list%data )
		             new%next => current%next
		             DEALLOCATE( list%data )
		             ALLOCATE( list%data, source=item )
		             list%next => new
		          END IF
		          EXIT search
		       END IF
            END IF
         ELSE
            !WRITE(error_unit,*) 'prev%data: ', allocated(prev%data)
            !WRITE(error_unit,*) 'values: ', item%value_of(), prev%data%value_of()
            !WRITE(error_unit,*) 'same (item, prev) ? ', same_type_as(item, prev%data)
           IF ( same_type_as(item, prev%data) ) THEN
               ALLOCATE( current )            ! insert at end
               ALLOCATE( current%data, source=item)
               prev%next => current
               EXIT search
            END IF
         END IF
         prev => current
         current => current%next
      END DO search
   END SUBROUTINE
   RECURSIVE SUBROUTINE write_fmt_list(dtv, unit, iotype, v_list, iostat, iomsg)
      CLASS(sorted_list), INTENT(in) :: dtv
      INTEGER, INTENT(in) :: unit, v_list(:)
      CHARACTER(len=*), INTENT(in) :: iotype
      INTEGER, INTENT(out) :: iostat
      CHARACTER(len=*), INTENT(inout) :: iomsg
      
      CHARACTER(len=2) :: next_component
      CHARACTER(len=1024) :: record
      
!      WRITE(error_unit,*) 'iotype in write_fmt_list ', trim(iotype)
      SELECT CASE (iotype)
      CASE ('LISTDIRECTED')
         WRITE(unit, fmt=*, iostat=iostat, iomsg=iomsg) &
               dtv%data
      CASE ('NAMELIST')
         IF ( associated(dtv%next) ) THEN
            WRITE(next_component, fmt='("T,")') 
         ELSE
            WRITE(next_component, fmt='("F")') 
         END IF
         WRITE(record, fmt=*) '"', &
               dtv%data%type_of(), '",', dtv%data, ',', trim(next_component)
         !WRITE(error_unit,*) 'YYY', trim(record)
         WRITE(unit, fmt=*, iostat=iostat, iomsg=iomsg) trim(record)
      CASE default
         iostat = 129
         iomsg = 'iotype ' // trim(iotype) // ' not implemented'
         RETURN
      END SELECT
      IF ( associated(dtv%next) ) THEN
         CALL write_fmt_list(dtv%next, unit, iotype, v_list, iostat, iomsg)
      END IF
   END SUBROUTINE
   SUBROUTINE read_fmt_list(dtv, unit, iotype, v_list, iostat, iomsg)
      CLASS(sorted_list), INTENT(inout) :: dtv
      INTEGER, INTENT(in) :: unit, v_list(:)
      CHARACTER(len=*), INTENT(in) :: iotype
      INTEGER, INTENT(out) :: iostat
      CHARACTER(len=*), INTENT(inout) :: iomsg
      
      INTEGER, PARAMETER :: maxlen=128
      CHARACTER(len=maxlen) :: type, value
      CLASS(sortable), ALLOCATABLE :: data
      LOGICAL :: next
      
      next = .true.
      DO WHILE (next)
         SELECT CASE (iotype)
         CASE ('NAMELIST')
            READ(unit, fmt=*, iostat=iostat, iomsg=iomsg) type 
           !IF ( iostat /= 0 ) RETURN    
            READ(unit, fmt=*, iostat=iostat, iomsg=iomsg) value, next 
            !   WRITE(error_unit, *) 'XXXX:', trim(type), ' ', trim(value)
            
            ! data = sortable(type, value)
            ! previous line replaced by the following two statements to work around compiler bugs
            IF ( allocated(data) ) DEALLOCATE( data )
            ALLOCATE( data, source=sortable(type, value) )
            !WRITE(error_unit, *) 'XXX:', data%type_of(), ' ', data%value_of()
            CALL add_to_sorted_list(dtv, data)
         CASE default
            iostat = 129
            iomsg = 'iotype ' // TRIM(iotype) // ' not implemented'
            RETURN
         END SELECT
      END DO
   END SUBROUTINE
   SUBROUTINE assign_sorted_list(to, from)
      TYPE(sorted_list), INTENT(in), TARGET :: from
      TYPE(sorted_list), INTENT(out), TARGET :: to     ! finalizer is executed on entry
      
      TYPE(sorted_list), POINTER :: p, q
      
      p => from; q => to
      
      deep_copy : DO
         IF ( associated(p) ) THEN
            q%data = p%data
         ELSE
            EXIT deep_copy
         END IF
         p => p%next
         if ( associated(p) ) ALLOCATE( q%next )
         q => q%next
      END DO deep_copy
   END SUBROUTINE
   TYPE(sorted_list) FUNCTION join_lists(list1, list2)
      TYPE(sorted_list), INTENT(in), TARGET :: list1, list2
      
      TYPE(sorted_list), POINTER :: p
      
      join_lists = list1
 
      p => list2
      process_list2items : DO
		IF ( associated(p) ) THEN
		   CALL add_to_sorted_list(join_lists, p%data)
		ELSE
		   EXIT process_list2items
		END IF
		p => p%next
      END DO process_list2items
   END FUNCTION
   LOGICAL FUNCTION lists_are_aliased(list1, list2)
      TYPE(sorted_list), INTENT(in), TARGET :: list1, list2
      
      TYPE(sorted_list), POINTER :: p1, p2
      
      lists_are_aliased = .false.
      p2 => list2
      process_list2 : DO
         p1 => list1
         process_list1 : DO
            IF ( associated(p1, p2) ) THEN
               lists_are_aliased = .true.
               EXIT process_list2
            END IF
            IF ( associated(p1) ) THEN
               p1 => p1%next
            ELSE
               EXIT process_list1
            END IF
         END DO process_list1
         IF ( associated(p2) ) THEN
            p2 => p2%next
         ELSE
            EXIT process_list2
         END IF
      END DO process_list2   
   END FUNCTION
   PURE RECURSIVE SUBROUTINE delete_sorted_list(list)
      TYPE(sorted_list), INTENT(inout) :: list
      
      IF ( associated(list%next) ) THEN
         DEALLOCATE( list%next )    ! invokes the finalizer recursively
      END IF
   END SUBROUTINE
END MODULE
PROGRAM exercise_sorted_list
   USE mod_sortable
   USE mod_sorted_list
   IMPLICIT none
   
   INTEGER, PARAMETER :: items = 7, maxlen = 8, strlen=128 
   CHARACTER(len=maxlen), PARAMETER :: str(items) = &
     [ "pears   ", "book    ", "apples  ", &
       "scissors", "spades  ", "peas    ", "carrots " ]
   CHARACTER(len=*), PARAMETER :: type_used = "sortable_string"
       
   CHARACTER(len=strlen) :: iomsg
   TYPE(sorted_list), ALLOCATABLE :: my_list    
   INTEGER :: listlen, iostat
   NAMELIST / my_namelist / listlen, my_list
     
   ALLOCATE( my_list ) ! necessary because overloaded assignment
                       ! suppresses auto-allocation
   work : BLOCK     
      CLASS(sortable), ALLOCATABLE :: array(:)    
      TYPE(sorted_list), ALLOCATABLE :: your_list
      TYPE(sorted_list) :: joint_list
      INTEGER :: i, iu
  
!      array = sortable([ (initialize(type_used, trim(str(i)(:))), i=1, items) ])
!     the above fails due to buggy compilers ...

      ALLOCATE( array(items), mold=sortable(initialize(type_used)) )
      DO i=1, items
         CALL array(i)%set(initialize(type_used, trim(str(i)(:))))
         !WRITE(*,*) array(i)
      END DO

! construct a list
      my_list = sorted_list(array)
      WRITE(*, fmt=*) 'Contents of my_list:'
      WRITE(*, fmt=*) my_list
      
! extend existing list
      CALL add_to_sorted_list(my_list, sortable(initialize(type_used, "aargh")))
      CALL add_to_sorted_list(my_list, sortable(initialize(type_used, "phoo")))
   
      WRITE(*, fmt=*) 'Contents of extended my_list:'
      WRITE(*, fmt=*) my_list
      
! Check namelist writing
      listlen = size(str) + 2
      OPEN(newunit=iu, form='FORMATTED', action='WRITE', status='UNKNOWN', &
           file='my_namelist.txt')
      WRITE(iu, nml=my_namelist)
      CLOSE(iu)
      
! small list without constructor
      ALLOCATE(your_list)
      CALL add_to_sorted_list(your_list, sortable(initialize(type_used,"eggs")))
      CALL add_to_sorted_list(your_list, sortable(initialize(type_used,"tongs")))

      WRITE(*, fmt=*) 'Contents of your_list:'
      WRITE(*, fmt=*) your_list
      
! concatenation of lists
      joint_list = your_list // my_list
      
      WRITE(*, fmt=*) 'Contents of joint_list:'
      WRITE(*, fmt=*) joint_list
      
! Check whether aliasing happens (it should not)
      WRITE(*, fmt=*) 'joint_list aliased with your_list? ', &
            lists_are_aliased(joint_list, your_list)
            
      joint_list = my_list
      WRITE(*, fmt=*) 'joint_list aliased with my_list after assignment? ', &
         lists_are_aliased(joint_list, my_list)
         
! Check reading the namelist after cleaning up the existing entries
      DEALLOCATE (my_list)
      ALLOCATE(my_list)
      listlen = 0   
      OPEN(newunit=iu, form='FORMATTED', action='READ', status='UNKNOWN', &
           file='my_namelist.txt')
      READ(iu, nml=my_namelist)
      WRITE(*, fmt=*) 'Contents of namelist after reread:'
      WRITE(*, fmt=*) listlen, my_list
      CLOSE(iu)
      
!  Move a list
      CALL move_alloc(your_list, my_list) 
      WRITE(*, fmt=*) 'Contents of my_list and allocation status of your_list after move:'
      WRITE(*, fmt=*) my_list
      WRITE(*, fmt=*) allocated(your_list)
      
!  Writing with DT should produce an error message
      WRITE(*, fmt='(DT"sorted_list_fmt"(10,2))', iostat=iostat, iomsg=iomsg) my_list
      WRITE(*, fmt=*) 'DT write failed with iostat ',iostat, ' and iomsg '
      WRITE(*, fmt=*, delim='quote') trim(iomsg)
   
   END BLOCK work  
END PROGRAM
