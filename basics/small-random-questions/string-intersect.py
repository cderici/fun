# given two strings, output a string containing chars found in both strings

def str_intersect_quad(s1,s2):
    ret = set()
    for c in s1:
        if c in s2:
            ret.add(c)
    return "".join(ret)

assert str_intersect_quad("caner", "nail") == "an"
assert str_intersect_quad("loot", "oops") == "o"
assert str_intersect_quad("", "oops") == ""
assert str_intersect_quad("s", "oopS") == ""

def str_intersect_linear(s1,s2):
    s2_set = set()
    for c in s2:
        s2_set.add(c)

    inter_set = set()
    for c in s1:
        if c in s2_set:
            inter_set.add(c)
    return "".join(inter_set)

assert str_intersect_linear("caner", "nail") == "an"
assert str_intersect_linear("loot", "oops") == "o"
assert str_intersect_linear("", "oops") == ""
assert str_intersect_linear("s", "oopS") == ""
