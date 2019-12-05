#include <cmath>
#include <vector>
#include <map>
#include <algorithm>
#include <numeric>
#include <limits>
#include <iostream>
#include <iterator>
#include <iomanip>
#include <array>

using Digit = int;
using number = std::array<Digit,6>;

void print(const number& n) {
  std::copy(n.cbegin(), n.cend(), std::ostream_iterator<Digit>(std::cout, " ")); std::cout << std::endl;
}

// increments a number at digit pos
number incpos(number n, const int pos) {
  n[pos]++;

  if (n[pos] > 9 && pos > 0) {
    n[pos] = 0;
    n = incpos(n, pos-1);
  }
  else if (pos == 0 && n[pos] >= 9) {
    n.fill(0);
  }
  return n;
}

// generates the smallest larger number than the input with monotone increasing digits
number init_inc(number n) {
  print(n);
  // TODO this does not work yet!
  std::adjacent_difference(n.begin(), n.end(), n.begin(),
                           [] (const Digit d1, const Digit d2)
                           {
                             Digit min, max;
                             std::cout << d1 << std::endl;
                             std::cout << d2 << std::endl;
                             std::tie(min,max) = std::minmax(d1,d2);
                             if (max == 9) return min;
                             else          return max;
                           });
  print(n);
  return n;
}

// generates the next larger number with nonotone increasing digits
number inc(const number& n) {
  return init_inc(incpos(n, n.size()-1));
}

bool hasRepetition(const number& n) {
  return std::adjacent_find(n.cbegin(), n.cend()) != n.cend();
}

int main() {
  const number first = {1,0,9,1,6,5};
  const number last  = {5,7,6,7,2,3};

  print(first);

  int count = 0;
  number n = init_inc(first);

  return 0;
  for (number n = init_inc(first); n <= last; n=inc(n)) {
  //if (hasRepetition(n)) print(n);
    count += hasRepetition(n);
  }

  std::cout << "Answer 1: " << count << std::endl;

  return 0;
}
