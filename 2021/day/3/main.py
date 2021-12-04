import fileinput

with fileinput.input() as f:
    N = 12
    numbers = [ln.strip() for ln in f]
    print(numbers)

    # Part 1
    gamma = 0
    epsilon = 0
    n0 = [0] * N
    n1 = [0] * N

    for i in range(N):
        for x in numbers:
            if x[i] == '0':
                n0[i] += 1
            elif x[i] == '1':
                n1[i] += 1

        if n0[i] < n1[i]:
            epsilon += 1 << (N - i - 1)
        else:
            gamma += 1 << (N - i - 1)

    print(gamma * epsilon)
        
    oxygen = set(numbers)
    
    for i in range(N):
        n0 = 0
        n1 = 0

        for s in oxygen:
            if s[i] == '0':
                n0 += 1
            elif s[i] == '1':
                n1 += 1

        if n0 > n1:
            oxygen = {s for s in oxygen if s[i] == '0'}
        else:
            oxygen = {s for s in oxygen if s[i] == '1'}

        if len(oxygen) == 1:
            break

    assert len(oxygen) == 1

    # co2
    co2 = set(numbers)

    for i in range(N):
        n0 = 0
        n1 = 0

        for s in co2:
            if s[i] == '0':
                n0 += 1
            elif s[i] == '1':
                n1 += 1

        if n0 <= n1:
            co2 = {s for s in co2 if s[i] == '0'}
        else:
            co2 = {s for s in co2 if s[i] == '1'}

        if len(co2) == 1:
            break

    assert len(co2) == 1

    ans2 = next(int(n, 2) for n in oxygen) * \
           next(int(n, 2) for n in co2)
    print(ans2)
