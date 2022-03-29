# Python

g_earth = 9.81
g_moon = 1.62
g_sun = 274.00
m_person = 149.68

gravity = [g_moon, g_earth, g_sun] 

forceN = [(m_person * i) for i in gravity]

print("In Newtons \nMoon: " + "{:.2f}".format(forceN[0]) + ", Earth: " + "{:.2f}".format(forceN[1]) + ", Sun: " + "{:.2f}".format(forceN[2]) + "\n")

forceLb = [i / 4.45 for i in forceN] 
print("In Pounds \nMoon: " + "{:.2f}".format(forceLb[0]) + ", Earth: " + "{:.2f}".format(forceLb[1]) + ", Sun: " + "{:.2f}".format(forceLb[2])) 

