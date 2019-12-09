#include "aoc02.h"

int main() {
  const Disk disk = readData("../aoc02.txt");
  //for (const auto i : disk) { std::cout << i << std::endl; }

  std::cout << "Answer 1: " << answer1(disk, 12, 2) << std::endl;
  std::cout << "Answer 2: " << answer2(disk, 19690720) << std::endl;

  return 0;
}

void test() {
  const std::vector<Int> test1{ 1,9,10,3, 2,3,11,0, 99, 30,40,50 };

  auto mem1 = convert(test1);

  if (!execute(mem1, Int{})) {
    std::cerr << "something went wrong!" << std::endl;
  }
  //for (const auto i : mem1) { std::cout << i.second << std::endl; }
}
