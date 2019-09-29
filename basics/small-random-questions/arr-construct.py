def constructArray(n, k):
    t = [i for i in range(1, n+1)]

    for j in range(1,k):
        t[j:] = t[j:][::-1]

    return t

print constructArray(5,2)

print constructArray(5,3)

print constructArray(5,4)
