      MODULE ITDLP_MOD
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
C
      CHARACTER(LEN=256) :: CSLINK
      LOGICAL :: IS_SYMLINK=.FALSE.
C
C        INTERFACE FOR C++ FUNCTION, ATOF 
C
      INTERFACE
         REAL(C_DOUBLE) FUNCTION ATOF(STR) BIND(C,NAME="atof")
         USE, INTRINSIC :: ISO_C_BINDING
         IMPLICIT NONE
         CHARACTER(LEN=1,KIND=C_CHAR), INTENT(IN) :: STR(*)
         END FUNCTION ATOF
      END INTERFACE
C
C        INTERFACE FOR C FUNCTION, SYMLINK
C
      INTERFACE
         SUBROUTINE SYMLINK(PATH1,PATH2) BIND(C,NAME="symlink")
         USE, INTRINSIC :: ISO_C_BINDING
         IMPLICIT NONE
         CHARACTER(LEN=1,KIND=C_CHAR), INTENT(IN) :: PATH1(*)
         CHARACTER(LEN=1,KIND=C_CHAR), INTENT(IN) :: PATH2(*)
         END SUBROUTINE SYMLINK
      END INTERFACE
C
C        INTERFACE FOR C FUNCTION, UNLINK
C
      INTERFACE
         SUBROUTINE UNLINK(PATH1) BIND(C,NAME="unlink")
         USE, INTRINSIC :: ISO_C_BINDING
         IMPLICIT NONE
         CHARACTER(LEN=1,KIND=C_CHAR), INTENT(IN) :: PATH1(*)
         END SUBROUTINE UNLINK
      END INTERFACE
C
C        INTERFACE FOR C FUNCTION, GETPID
C
      INTERFACE
         INTEGER(C_INT32_T) FUNCTION GETPID() BIND(C,NAME="getpid")
         USE, INTRINSIC :: ISO_C_BINDING
         END FUNCTION GETPID
      END INTERFACE
C
      END MODULE ITDLP_MOD
