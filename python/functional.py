from functools import reduce

num_list = [1,2,3,4,5]

print(list(filter(lambda x: x>2, num_list)))
print(list(map(lambda x: x**2, num_list)))
print(reduce(lambda x, y: x + y, num_list))

def gcd(a,b):
    if b == 0:
        return a
    else:
        return gcd(b, a%b)

def coprime(n):
    return [x for x in list(range(0,n)) if x < n and gcd(x,n) == 1]

print(coprime(10))
print(coprime(60))
