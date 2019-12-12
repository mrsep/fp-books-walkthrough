#include "aoc06.h"

int main() {
  const Disk disk = readData("../aoc06.txt");

  //for (const auto o_o : disk) { std::cout << o_o.first << " - " << o_o.second << std::endl;}
  const OrbitMap m = disk2map(disk);

  std::cout << "Answer1= " << answer1(m) << std::endl;
}
