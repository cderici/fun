# dec_to_bin(3) = '11'

"""
17 - 8 - 1
8 - 4 -  0
4 - 2 -  0   ^
2 - 1 -  0   |
1 - 0 -  1   |

18 - 9 - 0
9 - 4 -  1
4 - 2 -  0   ^
2 - 1 -  0   |
1 - 0 -  1   |

"""

def dec_to_bin(n):
    if n < 1:
        return False

    bin_str = ""
    while n > 0:
        bin_str = str(n&1) + bin_str
        n >>= 1
    return bin_str

assert dec_to_bin(0) == False
assert dec_to_bin(1) == '1'
assert dec_to_bin(2) == '10'
assert dec_to_bin(3) == '11'
assert dec_to_bin(17) == '10001'


# bin_to_decimal('11') == 3

def bin_to_decimal(bin_str):
    if bin_str == '':
        return False

    p = len(bin_str)-1
    dec = 0
    i = 0
    while p >= 0:
        dec += int(bin_str[p])*(2**i)
        i += 1
        p -= 1
    return dec

assert bin_to_decimal('11') == 3
assert bin_to_decimal('10001') == 17
assert bin_to_decimal('10010') == 18
assert bin_to_decimal('1010') == 10
assert bin_to_decimal('1011') == 11
