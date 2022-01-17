implicit none

real ::  v_0, rad, H, R, tt, t_t, i, t, g, x, y
integer :: ang

do
	write(*,*) 'Entre com os valores da velocidade inicial(m/s) e o ângulo (0° < theta < 90°) com a superfície horizontal: '
	read(*,*) v_0, ang
	if (ang <= 0) then
		print*
		print*, 'Entre com um ângulo válido !'
		print*
	else if (ang >= 90) then
		print*
		print*, 'Entre com um ângulo válido !' 
		print*
	else
		print* !pular uma linha
		print*, 'A altura máxima do projétil foi de', H(v_0, rad(ang)), 'm'
		print*, 'O alcance máximo do projétil foi de', R(v_0, rad(ang)), 'm'
		print*, 'O tempo total do projétil no ar foi de', tt(R(v_0, rad(ang)), v_0, rad(ang)), 's'
		exit
	end if
enddo

!Arquivo de saída com 100 linhas
t_t = tt(R(v_0, rad(ang)), v_0, rad(ang))
g = 9.8
open(1, file='tabela.txt')
print*, '     t(s)               x(m)             y(m)' !esboço no terminal
write(1,*)'     t(s)               x(m)             y(m)'

i=0
do	
	t = i*t_t/99
 	i = i + 1
 	x = v_0*cos(rad(ang))*t
 	y = v_0*sin(rad(ang))*t-(g*t**2)/2 
 	print*, t,x,y !esboço no terminal
 	write(1,*) t, x, y
 	if (i == 99) then
 		exit
 	end if
enddo
end program

!Funções importantes:

!Transformar graus para radianos:
real function rad(ang)
integer:: ang
real, parameter :: pi = acos(-1.) 
rad = ang*pi/180
end function rad

!Altura máxima
real function H(v0, rad)
real :: v0, g, rad
g = 9.8 
H = ((v0*sin(rad))**2)/(2*g)
end function H

!Alcance máximo
real function R(v0, rad)
real :: g, v0, rad
g = 9.8
R = (v0**2)*sin(2*rad)/g
end function R 

!Tempo total no ar
real function tt(dist,v0,rad)
real :: dist, v0, rad
tt = dist/(v0*cos(rad))
end function tt

