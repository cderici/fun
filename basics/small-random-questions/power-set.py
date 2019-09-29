# wihout empty

def power_set(ls):
    p_set = []

    for k in ls:
        extension = list(p_set)
        extension.append([])
        extension = [ns+[k] for ns in extension]
        p_set.extend(extension)

    return p_set

print power_set(['a','b','c'])
