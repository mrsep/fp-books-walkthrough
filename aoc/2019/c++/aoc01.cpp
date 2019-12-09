#include "aoc01.h"

#define CATCH_CONFIG_MAIN
#include "catch2/catch.hpp"

TEST_CASE("examples question 1", "[aoc01]") {
  REQUIRE(computeFuel(12)     ==     2);
  REQUIRE(computeFuel(14)     ==     2);
  REQUIRE(computeFuel(1969)   ==   654);
  REQUIRE(computeFuel(100756) == 33583);
}

TEST_CASE("examples question 2", "[aoc01]") {
  REQUIRE(computeTotalFuel(12)     ==     2);
  REQUIRE(computeTotalFuel(14)     ==     2);
  REQUIRE(computeTotalFuel(1969)   ==   966);
  REQUIRE(computeTotalFuel(100756) == 50346);
}

TEST_CASE("question 1", "[aoc01]") {
  REQUIRE(answer1() == 3380731);
}

TEST_CASE("question 2", "[aoc01]") {
  REQUIRE(answer2() == 5068210);
}
