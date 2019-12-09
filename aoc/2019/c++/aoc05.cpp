#include "aoc05.h"
#define CATCH_CONFIG_MAIN
//#include "catch2/catch.hpp"

int main() {
  const Disk disk = readData("../aoc05.txt");
  //for (const auto i : disk) { std::cout << i << std::endl; }

  std::cout << "Answer 1: "; answer(disk); std::cout << std::endl;
  std::cout << "Answer 2: "; answer(disk); std::cout << std::endl;

  return 0;
}

// TEST_CASE("example1", "[aoc05]") {
//   const Disk disk{1002,4,3,4,33};
//   Mem mem = convert(disk);
//   const Mem result{{0,1002}, {1,4}, {2,3}, {3,4}, {4,99}};

//   REQUIRE(run(mem, Int{}) == true);
//   REQUIRE(mem == result);
// }
