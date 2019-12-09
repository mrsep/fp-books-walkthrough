#include "aoc05.h"

int main() {
  const Disk disk = readData("../aoc05.txt");
  for (const auto i : disk) { std::cout << i << std::endl; }

  std::cout << "Answer 1: "; answer1(disk); std::cout << std::endl;

  return 0;
}
