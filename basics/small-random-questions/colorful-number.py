import numpy

"""
one approach is to use windows

3245

[3, 2, 4, 5]

w_size = 1
(3,2,4,5)
w_size = 2
(3,2,4,5,6,8,20)
w_size = 3
(3,2,4,5,6,8,20,24,40)
w_size = 4
(3,2,4,5,6,8,20,24,40,120)
True

[3,2,6]
w_size = 1
(3,2,6)
w_size = 2
False


we can also go with the power set approach
[5]
[[4], [4,5], [5]]
[[3], [3,4], [3,4,5], [3,5], [4], [4,5], [5]]

[[2], [2,3], [2,3,4], [2,3,4,5], [2,3,5], [2,4], [2,4,5], [2,5],
 [3], [3,4], [3,4,5], [3,5], [4], [4,5], [5]]

let's go with power sets
"""

def list_of_digits(N):
    p_of_ten = 0
    while (N / 10**p_of_ten) >= 10:
        p_of_ten += 1
    return _list_of_digits(N, p_of_ten)

def _list_of_digits(N, p_of_ten):
    if p_of_ten == 0:
        return [N]
    t = 10**p_of_ten
    return [N/t] + _list_of_digits(N % t, p_of_ten-1)


assert list_of_digits(5) == [5]
assert list_of_digits(45) == [4,5]
assert list_of_digits(245) == [2,4,5]
assert list_of_digits(3245) == [3,2,4,5]

"""
This redundantly uses exponential space, it can (and should) be
optimized.
We don't need to keep the previous subsets, only need to keep the
products of them as we go.
"""

def is_colorful(N):
    digits = list_of_digits(N)

    products = set()
    # create the power set while checking the products
    power_set = []
    for d in digits:
        # duplicate the power_set, call it extension
        extension = list(power_set)
        # put the [d] into the extension
        extension.append([])
        # append d to every element in the extension
        extension = [e + [d] for e in extension]
        # check the set of products with the products in the extension
        # append to the products if it's not there
        for e in extension:
            p = numpy.prod(e)
            if p in products:
                return False
            products.add(p)
        # extend power_set with the extension
        power_set.extend(extension)
    return True

assert is_colorful(3245) == True
assert is_colorful(326) == False
