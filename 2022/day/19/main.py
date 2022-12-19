from dataclasses import dataclass
from typing import Dict
import re


@dataclass(frozen=True)
class Resources:
    ore: int
    clay: int
    obsidian: int
    geode: int

    def __add__(self, other):
        return Resources(
            self.ore + other.ore,
            self.clay + other.clay,
            self.obsidian + other.obsidian,
            self.geode + other.geode,
        )

    def __sub__(self, other):
        return Resources(
            self.ore - other.ore,
            self.clay - other.clay,
            self.obsidian - other.obsidian,
            self.geode - other.geode,
        )

    def __le__(self, other):
        return (
            self.ore <= other.ore
            and self.clay <= other.clay
            and self.obsidian <= other.obsidian
            and self.geode <= other.geode
        )

    def __lt__(self, other):
        return (
            self.ore < other.ore
            and self.clay < other.clay
            and self.obsidian < other.obsidian
            and self.geode < other.geode
        )


ore_robot = Resources(1, 0, 0, 0)
clay_robot = Resources(0, 1, 0, 0)
obsidian_robot = Resources(0, 0, 1, 0)
geode_robot = Resources(0, 0, 0, 1)


@dataclass(frozen=True)
class Blueprint:
    ore_robot_cost: Resources
    clay_robot_cost: Resources
    obsidian_robot_cost: Resources
    geode_robot_cost: Resources

    def items(self):
        return {
            ore_robot: self.ore_robot_cost,
            clay_robot: self.clay_robot_cost,
            obsidian_robot: self.obsidian_robot_cost,
            geode_robot: self.geode_robot_cost,
        }


def solve(bp: Blueprint, t: int):
    max_resources = Resources(
        max(robot_cost.ore for robot_cost in bp.items().values()),
        max(robot_cost.clay for robot_cost in bp.items().values()),
        max(robot_cost.obsidian for robot_cost in bp.items().values()),
        max(robot_cost.geode for robot_cost in bp.items().values()),
    )
    DP = {}

    def dp(t: int, robots: Resources, resources: Resources):
        assert (
            resources.ore >= 0 and
            resources.clay >= 0 and
            resources.obsidian >= 0 and
            resources.geode >= 0
        )

        if t == 0:
            return resources.geode

        key = (
            resources.ore % max_resources.ore,
            resources.clay % max_resources.clay,
            resources.obsidian % max_resources.obsidian,
            resources.geode,
            robots,
            t,
        )

        if key in DP:
            return DP[key]

        DP[key] = a = max(
            dp(t - 1, robots + new_robot, resources + robots - robot_cost)
            for new_robot, robot_cost in list(bp.items().items()) + [(Resources(0, 0, 0, 0), Resources(0, 0, 0, 0))]
            if robot_cost <= resources
        )
        return a

    return dp(t, ore_robot, Resources(0, 0, 0, 0))


with open("input.txt") as f:
    ans1 = 0
    ans2 = 1

    for ln in f:
        bp_id, ore_robot_ore_cost, clay_robot_ore_cost, \
        obsidian_robot_ore_cost, obsidian_robot_clay_cost, \
        geode_robot_ore_cost, geode_robot_obsidian_cost = map(int, re.findall("\d+", ln))
        bp = Blueprint(
            Resources(ore_robot_ore_cost, 0, 0, 0),
            Resources(clay_robot_ore_cost, 0, 0, 0),
            Resources(obsidian_robot_ore_cost, obsidian_robot_clay_cost, 0, 0),
            Resources(geode_robot_ore_cost, 0, geode_robot_obsidian_cost, 0),
        )
        ans1 += bp_id * solve(bp, 24)

        if bp_id <= 3:
            ans2 *= solve(bp, 32)

print(ans1)
print(ans2)

