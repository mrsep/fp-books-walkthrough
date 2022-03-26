#include "aoc02.h"

int main() {
  const present p1{2,3,4};
  const present p2{1,1,10};

  std::cout << "2x3x4: wrap area=" << wrap_area(p1)
            << " ribbon length=" << ribbon_length(p1) << std::endl;
  std::cout << "1x1x10: wrap area=" << wrap_area(p2)
            << " ribbon length=" << ribbon_length(p2) << std::endl;

  std::cout << "Answer A: " << answer1() << std::endl; 
  std::cout << "Answer B: " << answer2() << std::endl; 

  return 0;
}
