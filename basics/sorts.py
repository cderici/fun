# quick sort

def q_sort(ls):
    return q_sort_iter(ls, 0, len(ls)-1)

def q_sort_iter(ls, left, right):
    if left < right:
        border_idx = partition(ls, left, right)
        q_sort_iter(ls, left, border_idx-1)
        q_sort_iter(ls, border_idx, right)
    return ls

def partition(ls, left, right):
    pivot_idx = (left+right)/2
    pivot = ls[pivot_idx]

    while left < right:

        while ls[left] < pivot:
            left += 1

        while ls[right] > pivot:
            right -= 1

        if left <= right:
            ls[left], ls[right] = ls[right], ls[left]
            left += 1
            right -= 1
    return left

assert q_sort([]) == []
assert q_sort([1]) == [1]
assert q_sort([2,1]) == [1,2]
assert q_sort([2,3,1]) == [1,2,3]
assert q_sort([4,2,3,1]) == [1,2,3,4]
assert q_sort([4,2,3,5,1]) == [1,2,3,4,5]
assert q_sort([4,2,3,6,5,1]) == [1,2,3,4,5,6]
assert q_sort([4,2,3,6,5,1,7]) == [1,2,3,4,5,6,7]
assert q_sort([4,2,3,8,6,5,1,7]) == [1,2,3,4,5,6,7,8]
assert q_sort([2,6,1,7,4,9,5]) == [1,2,4,5,6,7,9]
assert q_sort([6,1,7,4]) == [1,4,6,7]

def q_sort_rec(ls):
    if len(ls) <= 1:
        return ls
    pivot = ls[len(ls)/2]
    return q_sort_rec([x for x in ls if x < pivot]) + [x for x in ls if x == pivot] + q_sort_rec([x for x in ls if x > pivot])

assert q_sort_rec([]) == []
assert q_sort_rec([1]) == [1]
assert q_sort_rec([2,1]) == [1,2]
assert q_sort_rec([2,3,1]) == [1,2,3]
assert q_sort_rec([4,2,3,1]) == [1,2,3,4]
assert q_sort_rec([4,2,2,3,1]) == [1,2,2,3,4]
assert q_sort_rec([4,2,3,5,1]) == [1,2,3,4,5]
assert q_sort_rec([4,2,3,6,5,1]) == [1,2,3,4,5,6]
assert q_sort_rec([4,2,3,6,5,1,7]) == [1,2,3,4,5,6,7]
assert q_sort_rec([4,2,3,8,6,5,1,7]) == [1,2,3,4,5,6,7,8]
assert q_sort_rec([2,6,1,7,4,9,5]) == [1,2,4,5,6,7,9]
assert q_sort_rec([6,1,7,4]) == [1,4,6,7]
