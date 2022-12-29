C ============================================================================================== C
C ********************************************************************************************** C
C ************************                   INA                        ************************ C
C ************************          Useful FORTRAN Subroutines          ************************ C
C ************************                By: SaeednHT                  ************************ C
C ********************************************************************************************** C
C
      ! =================================================================================
      subroutine kvectordyad(A,B ,C)
      implicit none

      real*8,INTENT(in)  :: A(3),B(3)
      real*8,INTENT(out) :: C(3,3)
      INTEGER          :: i,j
      
      do i = 1,3
          do j=1,3
             C(i,j) = 0.0d0
          end do
      end do
            
      do i = 1,3
          do j=1,3
              C(i,j)=A(i)*B(j)
          end do
      end do
      
  
      return
      end subroutine
      ! =================================================================================
      ! =================================================================================
      subroutine kdyad(A,B, C)
      implicit none

      real*8,INTENT(in)  :: A(3,3),B(3,3)
      real*8,INTENT(out) :: C(3,3,3,3)
      INTEGER          :: i,j,k,l
      
      
      do i=1,3
          do j=1,3
              do k=1,3
                  do l=1,3
                      C(i,j,k,l) = 0.0d0
                  end do
              end do
          end do
      end do
      
      do i = 1,3
          do j=1,3
              do k=1,3
                  do l=1,3
                      C(i,j,k,l)=A(i,j)*B(k,l)
                  end do
              end do
          end do
      end do
      
  
      return
      end subroutine
      ! =================================================================================
      ! =================================================================================
      subroutine kdyadup(A,B, C)
      implicit none

      real*8,INTENT(in)  :: A(3,3),B(3,3)
      real*8,INTENT(out) :: C(3,3,3,3)
      INTEGER          :: i,j,k,l
      
      
      do i=1,3
          do j=1,3
              do k=1,3
                  do l=1,3
                      C(i,j,k,l) = 0.0d0
                  end do
              end do
          end do
      end do
      
      do i = 1,3
          do j=1,3
              do k=1,3
                  do l=1,3
                      C(i,j,k,l)=A(i,k)*B(j,l)
                  end do
              end do
          end do
      end do
      
  
      return
      end subroutine
      ! =================================================================================
      ! =================================================================================
      subroutine kdyaddown(A,B, C)
      implicit none

      real*8,INTENT(in)  :: A(3,3),B(3,3)
      real*8,INTENT(out) :: C(3,3,3,3)
      INTEGER          :: i,j,k,l
      
      
      do i=1,3
          do j=1,3
              do k=1,3
                  do l=1,3
                      C(i,j,k,l) = 0.0d0
                  end do
              end do
          end do
      end do
      
      do i = 1,3
          do j=1,3
              do k=1,3
                  do l=1,3
                      C(i,j,k,l)=A(i,l)*B(j,k)
                  end do
              end do
          end do
      end do
      
  
      return
      end subroutine
      ! =================================================================================
      ! =================================================================================
      subroutine kdoubledot22(A,B, C)
      implicit none
      real*8,INTENT(in)  :: A(3,3),B(3,3)
      real*8,INTENT(out) :: C
      INTEGER          :: i,j
      C = 0.0d0

          do i=1,3
              do j=1,3
                  C=C+A(i,j)*B(i,j)
              end do
          end do
          
  
      return
      end subroutine
      ! =================================================================================
      ! =================================================================================
      subroutine kdoubledot24(A,B, C)
      implicit none
      real*8,INTENT(in)  :: A(3,3),B(3,3,3,3)
      real*8,INTENT(out) :: C(3,3)
      INTEGER          :: i,j,k,l
                   
       C(1,1) = 0.0d0
       C(1,2) = 0.0d0
       C(1,3) = 0.0d0
       C(2,1) = 0.0d0
       C(2,2) = 0.0d0
       C(2,3) = 0.0d0
       C(3,1) = 0.0d0
       C(3,2) = 0.0d0
       C(3,3) = 0.0d0

          do k=1,3
              do l=1,3
                  do i=1,3
                      do j=1,3
                          C(k,l)=C(k,l)+A(i,j)*B(i,j,k,l)
!                          C(i,j)=C(i,j)+A(k,l)*B(k,l,i,j)
                      end do
                  end do
              end do
          end do
  
      return
      end subroutine
      ! =================================================================================
      ! =================================================================================
      subroutine kdoubledot42(A,B ,C)
      
      implicit none
      real*8,INTENT(in)  :: A(3,3,3,3),B(3,3)
      real*8,INTENT(out) :: C(3,3)
      INTEGER          :: i,j,k,l
      
                   
       C(1,1) = 0.0d0
       C(1,2) = 0.0d0
       C(1,3) = 0.0d0
       C(2,1) = 0.0d0
       C(2,2) = 0.0d0
       C(2,3) = 0.0d0
       C(3,1) = 0.0d0
       C(3,2) = 0.0d0
       C(3,3) = 0.0d0
          
          do i=1,3
              do j=1,3
                  do k=1,3
                      do l=1,3
                          C(i,j)=C(i,j)+A(i,j,k,l)*B(k,l)
                      end do
                  end do
              end do
          end do
          
      return
      end subroutine
      ! =================================================================================
      ! =================================================================================
      subroutine kdoubledot44(A,B, C)
      implicit none
      real*8,INTENT(in)  :: A(3,3,3,3),B(3,3,3,3)
      real*8,INTENT(out) :: C(3,3,3,3)
      INTEGER          :: i,j,k,l,m,n
      
      do i=1,3
          do j=1,3
              do k=1,3
                  do l=1,3
                      C(i,j,k,l) = 0.0d0
                  end do
              end do
          end do
      end do
      

          do i=1,3
              do j=1,3
                  do k=1,3
                      do l=1,3
                          do m=1,3
                              do n=1,3
                                  C(i,j,k,l)=C(i,j,k,l) 
     ,                            +A(i,j,m,n)*B(m,n,k,l)
                              end do
                          end do
                      end do
                  end do
              end do
          end do
          
      return
      end subroutine
      ! =================================================================================
      ! =================================================================================
      subroutine kpushforward44(LL,F ,ee)
      implicit none
      real*8,INTENT(in)  :: LL(3,3,3,3),F(3,3)
      real*8,INTENT(out) :: ee(3,3,3,3)
      REAL*8             :: FdyuF(3,3,3,3),FtdyuFt(3,3,3,3),ee1(3,3,3,3)
      INTEGER          :: i,j,k,l
      
      do i=1,3
          do j=1,3
              do k=1,3
                  do l=1,3
                      ee(i,j,k,l) = 0.0d0
                  end do
              end do
          end do
      end do
      
      CALL kdyadup(F,F ,FdyuF)
      CALL kdyadup(TRANSPOSE(F),TRANSPOSE(F) ,FtdyuFt)
      CALL kdoubledot44(FdyuF,LL ,ee1)
      CALL kdoubledot44(ee1,FtdyuFt ,ee)

      return
      end subroutine
      ! =================================================================================
      ! =================================================================================
      subroutine kdot21(a,A1 ,aprime)
      implicit none
      real*8,INTENT(in)  :: a(3),A1(3,3)
      real*8,INTENT(out) :: aprime(3)
      integer            :: i
      
      do i=1,3
          aprime(i) = 0.0d0
      enddo
      
      aprime(1) = A1(1,1) * a(1) + A1(1,2) * a(2) + A1(1,3) * a(3)
      aprime(2) = A1(2,1) * a(1) + A1(2,2) * a(2) + A1(2,3) * a(3)
      aprime(3) = A1(3,1) * a(1) + A1(3,2) * a(2) + A1(3,3) * a(3)

      return
      end subroutine
      ! =================================================================================
      ! =================================================================================
      subroutine ksym(A ,C)
      implicit none
      real*8,INTENT(in)  :: A(3,3)
      real*8,INTENT(out) :: C(3,3)
      integer             :: i,j  
      
      do i=1,3
          do j=1,3
              C(i,j)=0.0d0
          end do
      end do

      C = 0.5d0 *( A + TRANSPOSE(A) )

      return
      end subroutine
      ! =================================================================================