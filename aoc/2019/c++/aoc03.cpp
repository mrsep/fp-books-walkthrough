#include "aoc03.h"

int main() {
  //for (const auto i : disk) { std::cout << i << std::endl; }
  const Disk a = readData("../aoc03a.txt");
  const Disk b = readData("../aoc03b.txt");

  const Path path_a = disk2path(a);
  const Path path_b = disk2path(b);

  const Wire wire_a = path2wire(path_a, Eigen::Vector2i::Zero());
  const Wire wire_b = path2wire(path_b, Eigen::Vector2i::Zero());

  const Wire inter = wireInterSection(wire_a, wire_b);

  std::cout << "Answer 1: " << answer1(inter) << std::endl;
  std::cout << "Answer 2: " << answer2(wire_a, wire_b, inter) << std::endl;

  return 0;
}
