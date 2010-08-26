program invert
  implicit none

  real, allocatable, dimension(:,:) :: A         !  n x n Matrix
  real, allocatable, dimension(:,:) :: identity  !  n x n Matrix Einheitsmatrix

  integer :: n, m   !Dimension
  integer :: dim    !Dimension
  real :: Wert
  character(len=3) :: Matrixzeile ! Zeichenkette FIXME

  write(*,*) "Matrixrechnung v. 2010-08-25 (C) Giano und Jonas"
  write(*,*) "Dimension der n x n Matrix:"

  read *, dim
  allocate(A(dim, dim))
  allocate(identity(dim, dim))

  do n = 1, dim, 1 ! n lauft von 1 bis 4 mit Schrittweite 1
     do m = 1, dim, 1
        Wert = int(rand() * 10 + 0.5)  ! rand() = Zufallszahlengenerator (0..1)
900     format(2(I3,1x), 1x, F4.1)  ! https://srv.rz.uni-bayreuth.de/lehre/fortran90/vorlesung/V05/V05.html
        write(*, 900) n, m, Wert
        A(n, m)=Wert
     end do
  end do


  write(*,*) "===================================="


  do n = 1, dim, 1
     do m = 1, dim, 1
        if (n == m) then 
           identity(n, m) = 1 
        else 
           identity(n, m) = 0 
        endif
        write(*, 900) n, m, identity(n,m)
     end do
  end do


  n = 1 ! n-te Zeile mit 5 Multiplizieren
  do m = 1, dim, 1
     A(n,m)= A(n,m) * A(2,1)
  end do

 
 n = 2 
 do m = 1, dim, 1
     A(n,m)= A(n,m) * A(1,1)
  end do


 do m = 1, dim, 1
     A(1,m)= A(1,m) - A(2,m)
  end do







!Ausgeben
  do n = 1, dim, 1 
     do m = 1, dim, 1
        Wert = A(n,m)
        write(*, 900) n, m, Wert
     end do
  end do


end program invert










function showmatrix_serial(A,dim)
  integer n, m, dim

  do n = 1, dim, 1
     do m = 1, dim, 1
        write(*,*) 'n =', n
     end do
  end do

  return  
end function showmatrix_serial





