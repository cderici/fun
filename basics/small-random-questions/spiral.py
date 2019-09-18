"""
4
01 02 03 04
12 13 14 05
11 16 15 06
10 09 08 07

5
01 02 03 04 05
16 17 18 19 06
15 24 25 20 07
14 23 22 21 08
13 12 11 10 09

6     --->
  01 02 03 04 05 06
  20 21 22 23 24 07  |
^ 19 32 33 34 25 08  |
| 18 31 36 25 26 09  v
| 17 30 29 28 27 10
  16 15 14 13 12 11
    <----

directions:
[(0,1),(1,0),(0,-1),(-1,0)] we'll be wrapping around

current_number = 1

01 02 03 04 # row:0 - col:3
00 00 00 00
00 00 00 00
00 00 00 00

"""

def spiral(n):
    DIRECTIONS = [(0,1),(1,0),(0,-1),(-1,0)]
    current_direction = 0 # current_direction = (current_direction+1)%4
    current_number = 1

    row,col = 0,0
    sp = []
    for i in range(n):
        _sp = []
        for j in range(n):
            _sp.append(0)
        sp.append(_sp)

    while row < n and col < n and sp[row][col] == 0:
        # write the current number
        sp[row][col] = current_number
        current_number += 1

        # update the row & col
        # -- take the step
        row += DIRECTIONS[current_direction][0]
        col += DIRECTIONS[current_direction][1]
        # -- check if we're in bounds AND we see a 0 in the next spot
        if row >= n or col >= n or sp[row][col] != 0:
            # --if not
            # --- take a step back
            row -= DIRECTIONS[current_direction][0]
            col -= DIRECTIONS[current_direction][1]

            # --- and change the direction
            current_direction = (current_direction+1)%4

            # --- take a new step
            row += DIRECTIONS[current_direction][0]
            col += DIRECTIONS[current_direction][1]
    return sp

def print_spiral(n):
    sp = spiral(n)
    ret = ""
    for i in range(n):
        for j in range(n):
            ret += " %02d " % sp[i][j]
        ret += "\n"
    print ret

print 4
print_spiral(4)

print 5
print_spiral(5)

print 6
print_spiral(6)
