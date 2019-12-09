#include "aoc02.h"

#define CATCH_CONFIG_MAIN
#include "catch2/catch.hpp"

TEST_CASE("question 1", "[aoc02]") {
  const Disk disk = readData("../aoc02.txt");

  REQUIRE(answer1(disk, 12, 2) == 3101844);
}

TEST_CASE("question 2", "[aoc02]") {
  const Disk disk = readData("../aoc02.txt");

  REQUIRE(answer2(disk, 19690720) == 8478);
}

TEST_CASE("example 1", "[aoc02]") {
  const Disk disk{ 1,9,10,3, 2,3,11,0, 99, 30,40,50 };

  auto mem = convert(disk);

  const Mem result{{0,3500}, {1, 9}, { 2,10}, { 3,70},
                   {4,   2}, {5, 3}, { 6,11}, { 7, 0},
                   {8,  99}, {9,30}, {10,40}, {11,50}};

  REQUIRE(execute(mem, Int{}) == true);
  REQUIRE(mem == result);
}
