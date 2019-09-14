# changes are : 100 - 50 - 20 - 10 - 5 - 1

# 1) I give you N dollars, how many different changes are possible

CHANGES = [100,50,20,10,5]

def num_smaller_changes_divisible(n):
    sum = 0
    for c in CHANGES:
        if c <= n and n % c == 0:
            sum += 1
    return sum

def num_changes(n):
    if n <= 0:
        return False
    if n < 5:
        return 1

    return num_changes(n-1) + num_smaller_changes_divisible(n)

assert num_changes(1) == 1
assert num_changes(2) == 1
assert num_changes(3) == 1
assert num_changes(4) == 1
assert num_changes(5) == 2
assert num_changes(6) == 2
assert num_changes(7) == 2
assert num_changes(8) == 2 # 1:8, 5:1 & 1:3
assert num_changes(9) == 2 # 1:9, 5:1 & 1-4
assert num_changes(10) == 4 # 1:10, 5:1 & 1:5, 5:2, 10:1
assert num_changes(11) == 4 # 1:11, 5:1 & 1:6, 5:2 & 1:1, 10:1 & 1:1
assert num_changes(14) == 4 # 1:14, 5:1 & 1:9, 5:2 & 1:4, 10:1 & 1:4
assert num_changes(15) == 5 # 1:15, 5:1 & 1:10, 5:2 & 1:5, 10:1 & 1:5, 5:3
assert num_changes(19) == 5 # 1:19, 5:1 & 1:14, 5:2 & 1:9, 10:1 & 1:9, 5:3 & 1:4
assert num_changes(20) == 8 # 1:20, 5:1 & 1:15, 5:2 & 1:10, 10:1 & 1:10, 5:3 & 1:5, 5:4, 10:2, 20:1


"""
[1 - 4] : 1
[5 - 9] : 2
[10 - 14] : 4
[15 - 19] : 5
[20 - 24] : 8
[25 - 29] : 9
assert number_of_possible_changes(1) == 1
assert number_of_possible_changes(2) == 1
assert number_of_possible_changes(3) == 1
assert number_of_possible_changes(5) == 2
assert number_of_possible_changes(9) == 2
assert number_of_possible_changes(10) == 4
assert number_of_possible_changes(13) == 4
assert number_of_possible_changes(15) == 5
assert number_of_possible_changes(20) == 5 # ONE:20 - FIV:1/ONE:15 - FIV:2/
assert number_of_possible_changes(20) == 5
"""

# 2) I give you N dollars, produce the possible changes
"""
like $100 = [[1,100],[2,50],[[4,20]],[10,10],[20,5],[100,1],
             [[1,50],[2,20],[1,10]] ....]
"""

# 100 - 50 - 20 - 10 - 5 - 1
# HUN - FIF - TWE - TEN - FIV - ONE
# a little bit of overkill, but still
class HUN_O(object): None
HUN = HUN_O()

class FIF_O(object): None
FIF = FIF_O()

class TWE_O(object): None
TWE = TWE_O()

class TEN_O(object): None
TEN = TEN_O()

class FIV_O(object): None
FIV = FIV_O()

class ONE_O(object): None
ONE = ONE_O()

def increase_or_insert(solutions,obj,n,k):
    if n % k == 0:
        #print solutions
        for solution in solutions:
            if len(solution) == 1 and obj in solution:
                solution[obj] = solution[obj] + 1
                return
        solutions.append({obj:n/k})

def change(n):
    # stop 1
    if n <= 0:
        return False
    if n == 1:
        return [{ONE:1}]

    prev_solutions = change(n-1) # a set of solutions (hashmaps)

    for solution in prev_solutions:
        solution[ONE] = solution[ONE] + 1 if ONE in solution else 1
    #print "before %s" % prev_solutions
    increase_or_insert(prev_solutions,FIV,n,5)
    #print "after %s" % prev_solutions
    increase_or_insert(prev_solutions,TEN,n,10)
    increase_or_insert(prev_solutions,TWE,n,20)
    increase_or_insert(prev_solutions,FIF,n,50)
    increase_or_insert(prev_solutions,HUN,n,100)

    return prev_solutions

def print_change(for_n, changes):
    print "Changes for : %s" % for_n
    for ch in changes:
        print "(1) : %s - (5) : %s - (10) : %s - (20) : %s - (50) : %s - (100) : %s" % (
            ch[ONE] if ONE in ch else 0,
            ch[FIV] if FIV in ch else 0,
            ch[TEN] if TEN in ch else 0,
            ch[TWE] if TWE in ch else 0,
            ch[FIF] if FIF in ch else 0,
            ch[HUN] if HUN in ch else 0)

assert change(1) == [{ONE:1}]
assert change(2) == [{ONE:2}]
assert change(3) == [{ONE:3}]
assert change(4) == [{ONE:4}]
assert change(5) == [{ONE:5},{FIV:1}]
assert change(6) == [{ONE:6},{FIV:1,ONE:1}]
assert change(7) == [{ONE:7},{FIV:1,ONE:2}]
assert change(8) == [{ONE:8},{FIV:1,ONE:3}]
assert change(9) == [{ONE:9},{FIV:1,ONE:4}]
assert change(10) == [{ONE:10},{ONE:5,FIV:1},{FIV:2},{TEN:1}]
assert change(11) == [{ONE:11},{ONE:6,FIV:1},{FIV:2,ONE:1},{TEN:1,ONE:1}]
assert change(14) == [{ONE:14},{ONE:9,FIV:1},{FIV:2,ONE:4},{TEN:1,ONE:4}]
assert change(15) == [{ONE:15},{ONE:10,FIV:1},{FIV:2,ONE:5},{TEN:1,ONE:5},{FIV:3}]


def changes(n):
    print_change(n, change(n))
