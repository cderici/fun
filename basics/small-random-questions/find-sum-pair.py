# 6 - [1,3,4,5,6]

"""
put everthing in a hash table, make the indices start from 1 (instead of 0)
-- index start from one is part of the question description --

{1:1, 3:2, 4:3, 5:4, 6:5}

for each number i, look for N-i in the hash table
"""

def find_sum_pair(N, ls):
    ht_ls = {}
    for index, n in enumerate(ls):
        ht_ls[n] = index+1

    for index, n in enumerate(ls):
        if N-n in ht_ls:
            second_index = ht_ls[N-n]
            if second_index != index+1:
                return [index+1, second_index]
    return False

assert find_sum_pair(6, [1,3,4,5,6]) == [1,4] # for 1 and 5
