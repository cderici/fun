# () {} []

def isBalanced(br_str):
    stack = [] # push : append -- pop : pop -- top : stack[-1]

    open_brs = ["{","[","("]
    close_brs = ["}","]",")"]

    for i in br_str:
        if i in open_brs:
            stack.append(i)
        elif i in close_brs:
            top = stack[-1]
            # i needs to match the top
            if open_brs[close_brs.index(i)] != top:
                return False
            stack.pop()
        else:
            raise Exception("got an invalid character")
    return stack == []

assert isBalanced("{[()]}") == True

"""
emptystack == True  <- top
"""
assert isBalanced("{[(])}") == False

"""
{[(
"""

assert isBalanced("{[{[(())]}]}") == True
