#include "aoc06.h"

int main() {
  const Disk disk = readData("../aoc06.txt");

  const OrbitMap m = disk2map(disk);

  std::cout << "Answer1= " << answer1(m) << std::endl;
  std::cout << "Answer2= " << answer2(m) << std::endl;
}
