# Python

g_earth = 9.81
g_moon = 1.62
g_sun = 274

gravity = [g_earth, g_moon, g_sun]

m_person = 68

force = [m_person * i for i in gravity]

print(force)


