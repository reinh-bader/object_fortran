PROGRAM exercise_sortable
   USE mod_sortable
   CLASS(sortable), ALLOCATABLE :: s1, s2
   CHARACTER(len=128) :: str1, str2, typename
   
   WRITE(*,*) 'Enter desired extension type'
   READ(*,*) typename 
   WRITE(*,*) 'Enter two string values'
   READ(*,*) str1, str2

   s1 = sortable( initialize(trim(typename), trim(str1)) )
   s2 = sortable( initialize(trim(typename), trim(str2)) )
   ! allocation will only be performed if a supported
   ! type extension is supplied in typename
      
   IF ( allocated(s1) .AND. allocated(s2) ) THEN
      IF ( s1 < s2 ) THEN
         WRITE(*,*) 'First string precedes second string'
      ELSE
         WRITE(*,*) 'First string does not precede second string'
      END IF
   ELSE
      WRITE(*,*) 'At least one of the strings was not allocated.'
   END IF
   
END PROGRAM
   
