N = 29000000

# Part 1
elf = 1
ans = N
houses = [0] * (N // 10 + 1)
 
while elf < ans:
    for i in range(elf, N // 10 + 1, elf):
        houses[i] += elf * 10
        h = houses[i]
       
        if h >= N:
            ans = min(ans, i)
       
    elf += 1
   
print(ans)
 
# Part 2
elf = 1
ans = N
houses = {}
 
while elf < ans:
    for i in range(elf, 50 * elf + 1, elf):
        h = houses[i] = houses.setdefault(i, 0) + elf * 11
       
        if h >= N:
            ans = min(ans, i)
        
    elf += 1
   
print(ans)
