#include "aoc10.h"

int main() {
  const auto a_n = answer1();
  const Coord asteroid = a_n.first;
  const int detections = a_n.second;

  std::cout << "Answer1: Best asteroid= (" << asteroid(0) << ',' << asteroid(1) << ") with #detections= " << detections << std::endl;
  //std::cout << "Answer2= " << answer2() << std::endl;
  return 0;
}
