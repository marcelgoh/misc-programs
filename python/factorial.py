def rec_fact(x):
    if x == 0 or x == 1:
        return 1
    else:
        return x * rec_fact(x-1)

def iter_fact(x):
    acc = 1
    i = 1
    while (i <= x):
        acc = acc * i
        i = i + 1
    return acc

def for_fact(x):
    acc = 1
    for i in range(1, x+1):
        acc = acc * i
    return acc

def tail_fact(x):
    def helper(x, acc):
        if x == 1:
            return acc
        else:
            return helper(x-1, acc*x)
    return helper(x, 1)

