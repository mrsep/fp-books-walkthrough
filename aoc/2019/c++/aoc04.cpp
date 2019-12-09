#include "aoc04.h"
#define CATCH_CONFIG_MAIN
#include "catch2/catch.hpp"

TEST_CASE("questions", "[aoc04]") {
  const number first = {1,0,9,1,6,5};
  const number last  = {5,7,6,7,2,3};

  int answer1 = 0;
  int answer2 = 0;

  const auto start = std::chrono::high_resolution_clock::now();

  for (number n = init_inc(first); n <= last; n=inc(n)) {
    answer1 += hasRepetition(n);
    answer2 += hasExactRepetition(n);
  }
  const auto end = std::chrono::high_resolution_clock::now();

  REQUIRE(answer1 == 2814);
  REQUIRE(answer2 == 1991);

  std::cout << "Time= " << std::chrono::duration_cast<std::chrono::microseconds>(end - start).count() << "us\n";
}
