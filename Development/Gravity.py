# Python

g_earth = 9.81
g_moon = 1.62
g_sun = 274.00
m_person = 68.00

gravity = [g_moon, g_earth, g_sun] 

force = [m_person * i for i in gravity]

print(force) 

