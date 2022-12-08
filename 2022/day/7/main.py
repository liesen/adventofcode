from collections import defaultdict
from dataclasses import dataclass


@dataclass(frozen=True)
class Path:
    path: str

    def pushd(self, str):
        if self.path.endswith("/"):
            return Path(self.path + str)

        return Path(self.path + "/" + str)

    def popd(self):
        assert self.path != "/"
        return Path(self.path[0:self.path.rindex('/')])


@dataclass(frozen=True)
class Dir:
    name: str


@dataclass(frozen=True)
class File:
    name: str
    size: int


# File tree
tree = defaultdict(set)


with open("input.txt") as f:
    pwd = root = Path("/")
    lines = [ln.strip() for ln in f]
    i = 0

    while i < len(lines):
        ln = lines[i]

        if ln.startswith("$ cd "):
            dirname = ln[len("$ cd "):]

            if dirname.startswith("/"):
                pwd = Path(dirname)
            elif dirname == "..":
                pwd = pwd.popd()
            else:
                pwd = pwd.pushd(dirname)

            i += 1
        elif ln == "$ ls":
            i += 1

            while i < len(lines) and not lines[i].startswith("$"):
                ls_ln = lines[i]

                if ls_ln.startswith("dir "):
                    dirname = ls_ln[len("dir "):]
                    tree[pwd].add(Dir(dirname))
                else:
                    (fs, filename) = ls_ln.split(' ', maxsplit=2)
                    tree[pwd].add(File(filename, int(fs)))

                i += 1


def du(tree):
    from functools import cache
    
    @cache
    def du_entry(parent_path, entry):
        if isinstance(entry, File):
            return entry.size
        elif isinstance(entry, Dir):
            entry_path = parent_path.pushd(entry.name)
            return sum(
                du_entry(entry_path, e)
                for e in tree.get(entry_path, set())
            )

    return {
        path: sum(du_entry(path, entry) for entry in entries)
        for path, entries in tree.items()
    }


disk_usage = du(tree)

# Part 1
print(sum(n for n in disk_usage.values() if n <= 100_000))

# Part 2
total_size = disk_usage[root]
unused = 70_000_000 - total_size

print(
    next(
        n for n in sorted(disk_usage.values())
        if 70_000_000 - total_size + n >= 30_000_000
    )
)
