MODULE mod_sortable
   IMPLICIT none
   PRIVATE
   PUBLIC :: OPERATOR(<)
   TYPE, PUBLIC :: sortable
      CHARACTER(len=:), ALLOCATABLE :: string
   END TYPE
   
   INTERFACE OPERATOR(<)
      MODULE PROCEDURE less_than
   END INTERFACE
CONTAINS
   PURE LOGICAL FUNCTION less_than(s1, s2)
      TYPE(sortable), INTENT(in) :: s1, s2
      
      IF ( allocated(s1%string) .AND. allocated(s2%string) ) THEN
         less_than = llt(s1%string,s2%string)
      ELSE
         less_than = .false.    
      END IF
   END FUNCTION   
END MODULE
MODULE mod_sorted_list
   USE mod_sortable
   IMPLICIT none
   PRIVATE
   PUBLIC :: WRITE(formatted), READ(formatted), OPERATOR(//), ASSIGNMENT(=), &
	        add_to_sorted_list, lists_are_aliased
   TYPE, PUBLIC :: sorted_list
      PRIVATE
      TYPE(sortable) :: data
      TYPE(sorted_list), POINTER :: next => null()
   CONTAINS
      FINAL :: delete_sorted_list
   END TYPE
   
   INTERFACE sorted_list 
! the default constructor is unavailable because the type is opaque
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
   PURE FUNCTION create_sorted_list(item_array) RESULT(head)
      TYPE(sortable), INTENT(in) :: item_array(:)
      TYPE(sorted_list) :: head
      
      INTEGER :: i
      
      IF (size(item_array) == 0) RETURN
      
      DO i = 1, size(item_array)
         CALL add_to_sorted_list(head, item_array(i))
         ! handles tedious details of pointer fiddling
     END DO
   END FUNCTION
   PURE SUBROUTINE add_to_sorted_list(list, item)
      TYPE(sorted_list), INTENT(inout), TARGET :: list
      TYPE(sortable), INTENT(in) :: item
     
      TYPE(sorted_list), POINTER :: head, current, prev, new
   
      ! incoming list is virginal
      IF ( .NOT. allocated(list%data%string) ) THEN
          list%data = item
          RETURN
      END IF   
   
      head => list
      current => head
      prev => null()
      search : DO
         IF ( associated(current) ) THEN
            IF (item < current%data ) THEN ! insert before current
		       ALLOCATE( new )
		       IF ( associated(prev) ) THEN
       		      new%data = item
                  new%next => current
		          prev%next => new
		       ELSE                        ! adjust beginning of list
		          new%data = list%data
		          new%next => current%next
		          list%data = item
		          list%next => new
		       END IF
		       EXIT search
            END IF
         ELSE
            ALLOCATE( current )            ! insert at end
            current%data = item
            prev%next => current
            EXIT search
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
      
      IF ( associated(dtv%next) ) THEN
         WRITE(next_component, fmt='("T,")') 
      ELSE
         WRITE(next_component, fmt='("F")') 
      END IF
      SELECT CASE (iotype)
      CASE ('LISTDIRECTED')
         WRITE(unit, fmt=*, delim='quote', iostat=iostat, iomsg=iomsg) &
               dtv%data%string
      CASE ('NAMELIST')
         WRITE(unit, fmt=*, iostat=iostat, iomsg=iomsg) '"', &
               dtv%data%string, '",', trim(next_component)
      CASE default
         iostat = -129
         iomsg = 'iotype ' // trim(iotype) // ' not implemented'
         RETURN
      END SELECT
      IF ( associated(dtv%next) ) THEN
         CALL write_fmt_list(dtv%next, unit, iotype, v_list, iostat, iomsg)
      END IF
   END SUBROUTINE
   RECURSIVE SUBROUTINE read_fmt_list(dtv, unit, iotype, v_list, iostat, iomsg)
      CLASS(sorted_list), INTENT(inout) :: dtv
      INTEGER, INTENT(in) :: unit, v_list(:)
      CHARACTER(len=*), INTENT(in) :: iotype
      INTEGER, INTENT(out) :: iostat
      CHARACTER(len=*), INTENT(inout) :: iomsg
      
      INTEGER, PARAMETER :: maxlen=128
      CHARACTER(len=maxlen) :: str
      LOGICAL :: next
      
      SELECT CASE (iotype)
      CASE ('NAMELIST')
         READ(unit, fmt=*, iostat=iostat, iomsg=iomsg) str, next
         CALL add_to_sorted_list(dtv, sortable(trim(str)))
      CASE default
         iostat = -129
         iomsg = 'iotype ' // TRIM(iotype) // ' not implemented'
         RETURN
      END SELECT
      IF ( next ) THEN
         CALL read_fmt_list(dtv, unit, iotype, v_list, iostat, iomsg)
      END IF
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
         if ( associated(p) ) allocate(q%next)
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
       
   CHARACTER(len=strlen) :: iomsg
   TYPE(sorted_list), ALLOCATABLE :: my_list    
   INTEGER :: listlen, iostat
   NAMELIST / my_namelist / listlen, my_list
     
   ALLOCATE( my_list )
   work : BLOCK     
      TYPE(sortable) :: array(items)    
      TYPE(sorted_list), ALLOCATABLE :: your_list
      TYPE(sorted_list) :: joint_list
      INTEGER :: i, iu
  
      DO i=1, items
         array(i) = sortable( trim(str(i)(:)) )
      END DO
      
! construct a list
      my_list = sorted_list(array)
      WRITE(*, fmt=*) 'Contents of my_list:'
      WRITE(*, fmt=*) my_list
   
! extend existing list
      CALL add_to_sorted_list(my_list, sortable("aargh") )
      CALL add_to_sorted_list(my_list, sortable("phoo") )
   
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
      CALL add_to_sorted_list(your_list, sortable("eggs") )
      CALL add_to_sorted_list(your_list, sortable("tongs") )

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
