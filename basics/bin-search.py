# binary search

def bin_search_rec(ls, n):
    if ls == []:
        return False

    mid_idx = len(ls)/2
    mid = ls[mid_idx]
    if n == mid:
        return True
    elif n > mid:
        return bin_search_rec(ls[mid_idx:], n)
    else:
        return bin_search_rec(ls[:mid_idx], n)

assert bin_search_rec([], 2) == False
assert bin_search_rec([2], 2) == True
assert bin_search_rec([1,2], 2) == True
assert bin_search_rec([2,3], 2) == True
assert bin_search_rec([1,2,3], 2) == True
assert bin_search_rec([1,2,3], 3) == True
assert bin_search_rec([1,2,3], 1) == True

def bin_search_iter(ls, n):
    if ls == []:
        return False

    left = 0
    right = len(ls)
    while left < right:
        mid_idx = (left+right)/2
        mid = ls[mid_idx]
        if n == mid:
            return True # could return mid_idx
        elif n > mid:
            left = mid_idx
        else:
            right = mid_idx
    return False # could return -1 for not found

assert bin_search_iter([], 2) == False
assert bin_search_iter([2], 2) == True
assert bin_search_iter([1,2], 2) == True
assert bin_search_iter([2,3], 2) == True
assert bin_search_iter([1,2,3], 2) == True
assert bin_search_iter([1,2,3], 3) == True
assert bin_search_iter([1,2,3], 1) == True
