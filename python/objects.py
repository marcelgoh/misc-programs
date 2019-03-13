def Person(name,age):
    _name = name
    _age = age

    def birthday():
        nonlocal _age
        _age += 1
    def rename(new_name):
        nonlocal _name
        _name = new_name
    def getName():
        return _name
    def getAge():
        return _age
    def callMethod(method_name):
        if method_name == "birthday":
            return birthday
        elif method_name == "rename":
            return rename
        elif method_name == "getName":
            return getName
        elif method_name == "getAge":
            return getAge
        else:
            print("Method not found")
    return callMethod

s = Person("sam", 19)
print(s("getName")())  # prints "sam"
s("rename")("steve")
print(s("getName")())  # prints "steve"
print(s("getAge")())   # prints "19"
s("birthday")()
print(s("getAge")())   # prints "20"

