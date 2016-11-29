
program temp_converter !Converts temperature units from Fahrenheit

implicit none
real 	:: x, fx1, fx2

print*, "Enter temperature in Farehheit"
read*, x

fx1 =  (5.0/9.0)*(x-32.0)
fx2 = 273.15 + x

print*, "Celcius"
print*, fx1

print*, "Kelvin"
print*, fx2

end program temp_converter

