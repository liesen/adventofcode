import fileinput
import turtle

# Turtles
screen = turtle.Screen()
ll = turtle.Vec2D(-342.00,-7272.00)
llx, lly = ll
ur = turtle.Vec2D(29643.00,47487.00)
urx, ury = ur
screen.setworldcoordinates(llx, lly, urx, ury)

# Heroes in a half shell!
turtle1 = turtle.Turtle()
turtle1.color('red')

turtle2 = turtle.Turtle()
turtle2.color('blue')

w = turtle.Vec2D(10, 1)

# Turtle power!
for line in fileinput.input('input.txt'):
    d, i = line[0], int(line[1:])

    if d == 'N':
        turtle1.goto(turtle1.position() + (0, -i))
        w += (0, i)
    elif d == 'S':
        turtle1.goto(turtle1.position() + (0, i))
        w += (0, -i)
    elif d == 'E':
        turtle1.goto(turtle1.position() + (i, 0))
        w += (i, 0)
    elif d == 'W':
        turtle1.goto(turtle1.position() + (-i, 0))
        w += (-i, 0)
    elif d == 'L':
        turtle1.left(360 - i)  # -y is north
        w = w.rotate(i)
    elif d == 'R':
        turtle1.right(360 - i)  # -y is north
        w = w.rotate(360 - i)
    elif d == 'F':
        turtle1.forward(i)
        turtle2.goto(turtle2.position() + w * i)

x1, y1 = turtle1.position()
print(abs(round(x1)) + abs(round(y1)))

x2, y2 = turtle2.position()
print(abs(round(x2)) + abs(round(y2)))
screen.getcanvas().postscript(file='cowabunga.ps')
