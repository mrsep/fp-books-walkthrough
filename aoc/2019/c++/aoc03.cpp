#include "aoc03.h"

#define CATCH_CONFIG_MAIN
#include "catch2/catch.hpp"

TEST_CASE("question 1", "[aoc03]") {
  const Disk a = readData("../aoc03a.txt");
  const Disk b = readData("../aoc03b.txt");

  const Wire wire_a = path2wire(disk2path(a), Eigen::Vector2i::Zero());
  const Wire wire_b = path2wire(disk2path(b), Eigen::Vector2i::Zero());

  const Wire inter = wireInterSection(wire_a, wire_b);

  REQUIRE(answer1(inter) == 293);
}

TEST_CASE("question 2", "[aoc03]") {
  const Disk a = readData("../aoc03a.txt");
  const Disk b = readData("../aoc03b.txt");

  const Wire wire_a = path2wire(disk2path(a), Eigen::Vector2i::Zero());
  const Wire wire_b = path2wire(disk2path(b), Eigen::Vector2i::Zero());

  const Wire inter = wireInterSection(wire_a, wire_b);

  REQUIRE(answer2(wire_a, wire_b, inter) == 27306);
}
