import math

# a function that divides n length wire to 1 length wire with minimum cut
def cut_min(n : int) -> int:
    return math.ceil(math.log2(n))

print(cut_min(5))
print(cut_min(8))
print(cut_min(100))