def find_marker(s, marker_len):
    return next(
        i + marker_len
        for i in range(0, len(s) - marker_len)
        if len(set(s[i:i + marker_len])) == marker_len
    )
   
 
assert find_marker("bvwbjplbgvbhsrlpgdmjqwftvncz", 4) == 5
assert find_marker("nppdvjthqldpwncqszvftbrmjlhg", 4) == 6
assert find_marker("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 4) == 10
assert find_marker("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 4) == 11
 
with open("input.txt") as f:
    for ln in f:
        print(find_marker(ln, 4))
        print(find_marker(ln, 14))