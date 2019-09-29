
def a_index(s1,s2):
    indices = []
    l_s1 = len(s1)
    l_s2 = len(s2)
    if l_s1 < l_s2:
        return indices

    anagram_h = 0
    for c in s2:
        anagram_h += hash(c)

    current_h = 0
    for c in s1[:l_s2]:
        current_h += hash(c)
    if current_h == anagram_h: indices.append(0)
    i = 1
    while i + l_s2 <= l_s1: # slide the window
        current_h -= hash(s1[i-1])
        current_h += hash(s1[i+l_s2-1])
        if current_h == anagram_h:
            indices.append(i)
        i += 1

    return indices

"""
anagram_h = X

bcaacb
i = 1
current_h = caa
i = 2
current_h = aac
i = 3
current_h = acb [0,3]
i = 4 stop

bca
i = 0
current_x = X
[0]
i = 1 stop

"""
assert a_index("bca","abc") == [0]
assert a_index("bcaacb","abc") == [0,3]
assert a_index("cbaebabacd","abc") == [0,6]
assert a_index("bc","abc") == []
assert a_index("bckpa","abc") == []
