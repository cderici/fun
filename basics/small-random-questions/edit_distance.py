# one edit apart

# Write a function that returns whether two words are exactly "one edit" away using the following signature:

def oneEditApart(s1,s2):
    l1 = len(s1)
    l2 = len(s2)
    if l2 > l1:
        return oneEditApart(s2,s1)
    if l1 - l2 > 1:
        return False

    p1, p2 = 0, 0
    is_edited = False
    while p1 < l1:
        if p2 >= l2 and not is_edited:
            # we are at the very end and we still have one edit left
            # so just insert
            return True

        """
        this logic can be refactored
        """
        if s1[p1] == s2[p2]:
            p1 += 1
            p2 += 1
        else:
            if is_edited:
                return False
            if l1 > l2: # means edit is insert
                # simulate an insert
                p1 += 1
            else: # means edit is replace
                # simulate a replace
                p1 += 1
                p2 += 1
            is_edited = True
    return True

assert not oneEditApart("cat", "dog")
assert oneEditApart("cat", "cats") == oneEditApart("cats", "cat")
assert oneEditApart("cat", "cut")
assert oneEditApart("cat", "cast") == oneEditApart("cast", "cat")
assert oneEditApart("cat", "at")
assert not oneEditApart("cat", "act")
