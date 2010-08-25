program invert
  implicit none

  real, allocatable, dimension(:,:) :: A         !  n x n Matrix

  integer :: n, m   !Dimension
  integer :: dim    !Dimension
  real :: Wert
  character(len=3) :: Matrixzeile ! Zeichenkette FIXME

  write(*,*) "Matrixrechnung v. 2010-08-25 (C) Giano und Jonas"
  write(*,*) "Dimension der n x n Matrix:"

  read *, dim
  allocate(A(dim, dim))

  do n = 1, dim, 1 ! n lauft von 1 bis 4 mit Schrittweite 1
     do m = 1, dim, 1
        Wert = int(rand() * 10 + 0.5)  ! rand() = Zufallszahlengenerator (0..1)
900     format(2(I3,1x), 1x, F3.1)  ! https://srv.rz.uni-bayreuth.de/lehre/fortran90/vorlesung/V05/V05.html
        write(*, 900) n, m, Wert
        A(n, m)=Wert
     end do
  end do

write (*,*) identity(3)


end program invert


function  identity(dimmi)
  implicit none
  integer :: n, m 
  integer, intend ( in ) :: dimmi
  real, allocatable, dimension(:,:) :: identity
  allocate(identity(dimmi, dimmi))

  do n = 1, dimmi, 1 ! n lauft von 1 bis 4 mit Schrittweite 1
     do m = 1, dimmi, 1
        if (n == m) then 
           identity(n, m) = 1 
        else 
           identity(n, m) = 0 
        endif
     end do
  end do
  write (*,*) identity
  return
end function identity

     ! function rowmul(A, row, factor) 



     function showmatrix_serial(A,dim)
       integer n, m, dim

       do n = 1, dim, 1
          do m = 1, dim, 1
             write(*,*) 'n =', n
          end do
       end do

       return  
     end function showmatrix_serial





