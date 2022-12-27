C FORTRAN SUBROUTINE TO BE OPEN WITH R      
C README: FROM R INPUT x,y, HAVE DIMENSION n, AND MATRIX d, DIM(n,n), a IS AN ESCALAR

      SUBROUTINE distaf1(i, n, x, y, d, a, dij)
      IMPLICIT NONE

      INTEGER i, j, n
      REAL*8 a
      DOUBLE PRECISION x(n), y(n), d(n,n), dij(n)

      
      DO j=1, n
       IF(i.ne.j)THEN
          dij(j)= d(i,j)*a
       ENDIF
      ENDDO
      
      RETURN
      END SUBROUTINE distaf1

