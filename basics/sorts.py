# quick sort

# in-place
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

# if we're ok with using a lot of space
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


# merge sort

# O(nlogn) space
# could be made to use O(n) space
def m_sort(ls):
    if len(ls) <= 1:
        return ls
    mid_idx = len(ls)/2
    return merge(m_sort(ls[:mid_idx]), m_sort(ls[mid_idx:]))

def merge(l1,l2):
    ret = []
    # like sorted card decks
    while l1 != [] and l2 != []:
        if l1[0] < l2[0]:
            ret.append(l1.pop(0))
        else:
            ret.append(l2.pop(0))

    # either l1 or l2 might still be non-empty
    if l1 != []:
        ret.extend(l1)
    if l2 != []:
        ret.extend(l2)

    return ret

assert m_sort([]) == []
assert m_sort([1]) == [1]
assert m_sort([2,1]) == [1,2]
assert m_sort([2,3,1]) == [1,2,3]
assert m_sort([4,2,3,1]) == [1,2,3,4]
assert m_sort([4,2,2,3,1]) == [1,2,2,3,4]
assert m_sort([4,2,3,5,1]) == [1,2,3,4,5]
assert m_sort([4,2,3,6,5,1]) == [1,2,3,4,5,6]
assert m_sort([4,2,3,6,5,1,7]) == [1,2,3,4,5,6,7]
assert m_sort([4,2,3,8,6,5,1,7]) == [1,2,3,4,5,6,7,8]
assert m_sort([2,6,1,7,4,9,5]) == [1,2,4,5,6,7,9]
assert m_sort([6,1,7,4]) == [1,4,6,7]
