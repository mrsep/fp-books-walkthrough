#include "aoc04.h"

int main() {
  const number first = {1,0,9,1,6,5};
  const number last  = {5,7,6,7,2,3};

  print(first);

  int answer1 = 0;
  int answer2 = 0;

  const auto start = std::chrono::high_resolution_clock::now();

  for (number n = init_inc(first); n <= last; n=inc(n)) {
    answer1 += hasRepetition(n);
    answer2 += hasExactRepetition(n);
  }
  const auto end = std::chrono::high_resolution_clock::now();

  std::cout << "Answer 1: " << answer1 << std::endl;
  std::cout << "Answer 2: " << answer2 << std::endl;

  std::cout << "Time= " << std::chrono::duration_cast<std::chrono::microseconds>(end - start).count() << "us\n";

  return 0;
}
