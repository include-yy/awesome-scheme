constraint = [lambda c: True] * 10
constraint[0] = lambda c: c[0] == 10
constraint[4] = lambda c: c[0] + c[1] == c[3] + c[4]
constraint[6] = lambda c: c[2] + c[3] == c[5] + c[6]
constraint[8] = lambda c: c[4] + c[5] == c[7] + c[8]
constraint[9] = lambda c: c[6] + c[7] == c[1] + c[9]

sides, rots = [0,1,2,3,2,4,5,4,6,7,6,8,9,8,1], []
for i in range(0, len(sides), 3): rots.append(sides[i:] + sides[:i])

search, sols = [[]], []
while len(search) > 0:
    c = search.pop()
    left = set(range(1, 11)) - set(c)
    if len(left) == 0: sols.append(c)
    for cv in left:
        if constraint[len(c)](c + [cv]): search.append(c + [cv])

for sol in sols:
    sol[:] = min([sol[cidx] for cidx in rot] for rot in rots)

print reduce(lambda a, b: a + b, map(str, max(sols)))
