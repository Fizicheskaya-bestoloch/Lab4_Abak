module abak
contains
	subroutine DF(N, Nmax, r0, M, NComp)
		implicit none
		integer :: i, Nmax
		real(8) :: N(Nmax), NComp(Nmax)
		real(8) :: r0,r,g,pi,m
		pi = atan(1.0)*4.0

		do i=1, Nmax !генерирование Nmax чисел компьютером
			r = (pi + 15*r0)
			g = r - floor(r)!
			r0 = g! g запись деситичная
			g=g*100! чтобы перед запятой вынеслось 2 числа
			NComp(i)=nint(g)! округляем эти 2 числа ,получаем случайное число до 999
		end do
		r = (pi + 15*r0) !генерирование числа m
		g = r - floor(r)
		r0 = g
		g=g*100
		m= nint(g)
	end subroutine
end module
program abakp
use abak
implicit none
integer, parameter:: Nmax=5
integer :: i
real(8) :: N(Nmax), NComp(Nmax)!массив куда записваются чичла пользователя,массив куда записываются числа компьютера
real(8) :: r0, SUMMComp, SUMM, M
print*, 'enter the', Nmax, 'numbers'
do i=1,Nmax
	read*, N(i)
end do
print*, 'enter r0, 0<r0<1'
read*, r0
call DF(N, Nmax, r0, M, NComp)!
print*, 'Your numbers: '
do i=1, Nmax
	print*, N(i)
	SUMM = SUMM + N(i)
end do
print*, 'Your SUMM: ', SUMM
print*, 'Numbers of computer: '
do i=1, Nmax
	print*, NComp(i)
	SUMMComp = SUMMComp + NComp(i)
end do
print*, 'SUMM of computer: ', SUMMComp
print*, 'AND THE NUMBER IS:', M 
	if (abs(M-SUMM) .LT. abs(M-SUMMComp)) then
		print*, 'You won ! :)'
	else if (abs(M-SUMM) .EQ. abs(M-SUMMComp)) then
		print*, 'DRAW !'
	else
		print*, 'Humanity lost ! :('
end if
end program
