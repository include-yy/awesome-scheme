x = 1
y = 1
limit = 1000000000000

while True:
    if x % 2 == 1 and y % 2 == 1:
        if (x + 1) / 2 > limit:
            print ((y + 1) / 2)
            break
    x, y = 3 * x + 4 * y, 2 * x + 3 * y