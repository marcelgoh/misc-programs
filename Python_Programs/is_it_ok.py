# Written by Marcel Goh on 19 May 2018
# A program to tell you whether it is OK or not.

print("Is it okay?")
print("Enter your age:")
your_age = input()
print("Enter his/her age:")
their_age = input()
okay = 0
if your_age > their_age:
    if int(their_age) >= (int(your_age)/2 + 7):
        okay = 1
else:
    if int(your_age) >= (int(their_age)/2 + 7):
        okay = 1
if okay == 1:
    print("OK!")
else:
    print("Eww.")
