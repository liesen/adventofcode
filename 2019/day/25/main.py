from intcode import Intcode
from itertools import chain, combinations
import networkx as nx
from networkx.drawing.nx_agraph import write_dot
from collections import namedtuple


with open('input.txt') as fp:
    prog = Intcode([int(x) for x in fp.read().split(',')])


Room = namedtuple('Room', ['name', 'desc', 'doors', 'items'])


def read_output(prog):
    output = ''

    while prog.outputs:
        output += chr(prog.outputs.popleft())
    
    return output


def parse_room(prog):
    output = read_output(prog)
    
    assert not prog.outputs
    lines = output.strip().splitlines()
    i = 0

    assert lines[i].startswith('== '), lines[i]
    name = lines[i][len('== '):-len(' ==')]
    i += 1

    desc = ''

    while lines[i]:
        desc += lines[i]
        i += 1

    i += 1

    doors = []
    assert lines[i] == 'Doors here lead:', lines[i]
    i += 1

    while lines[i]:
        assert lines[i].startswith('- '), lines[i]
        doors.append(lines[i][len('- '):])
        i += 1

    assert not lines[i]
    i += 1

    items = []
    
    if lines[i] == 'Command?':
        return Room(name=name, desc=desc, doors=doors, items=items)

    if lines[i] == 'Items here:':
        i += 1
        
        while lines[i]:
            assert lines[i].startswith('- '), lines[i]
            items.append(lines[i][len('- '):])
            i += 1

    return Room(name=name, desc=desc, doors=doors, items=items)


def parse_inv(prog):
    output = read_output(prog)
    lines = output.strip().splitlines()
    i = 0
    assert lines[i] == 'Items in your inventory:'
    i += 1
    inv = []

    while lines[i].startswith('- '):
        inv.append(lines[i][len('- '):])
        i += 1

    return inv


prog.run()
start = parse_room(prog)
G = nx.DiGraph()

reverse = {'north': 'south', 'south': 'north', 'east': 'west', 'west': 'east'}

def command(s):
    return [ord(x) for x in s] + [10]


INV = ['mug', 'easter egg', 'shell', 'weather machine', 'festive hat', 'whirled peas', 'sand', 'space heater']


def explore(prog, room, G):
    if room.name in G.nodes:
        return
    
    G.add_node(room.name, shape='ellipse', label=f'<{room.name}<br/>{room.desc}>')

    # Take everything
    for item in room.items:
        G.add_edge(room.name, item)

        if item in INV:
            G.add_node(item, shape='rect')
            prog.runs(f'take {item}')
            prog.outputs.clear()
        else:
            G.add_node(item, shape='octagon')

    for door in room.doors:
        prog.run(command(door))
        new_room = parse_room(prog)
        explore(prog, new_room, G)
        G.add_edge(room.name, new_room.name, label=door)
        prog.runs(reverse[door])
        prog.outputs.clear()


explore(prog, start, G)
write_dot(G, 'map.gv')

# Go to Security Checkpoint
path = ['east', 'east', 'south', 'west', 'south', 'west', 'south']

for step in path:
    prog.runs(step)
    output = read_output(prog)


def powerset(iterable):
    s = list(iterable)
    return chain.from_iterable(combinations(s, r) for r in range(len(s)+1))


for inv_set in powerset(INV):
    for item in inv_set:
        prog.runs(f'drop {item}')
        prog.outputs.clear()

    prog.runs('south')
    output = read_output(prog)

    if prog.halt:
        print(output)
        break

    for item in inv_set:
        prog.runs(f'take {item}')
        prog.outputs.clear()

