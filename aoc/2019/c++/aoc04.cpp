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
  // find the first digit which is greater than the next digit
  auto it = std::adjacent_find(n.begin(), n.end(),
                               [] (const Digit d1, const Digit d2)
                               { return d1 > d2; });

  // fill the remaining digits with the value at it
  std::fill(it, n.end(), *it);

  return n;
}

// generates the next larger number with nonotone increasing digits
number inc(const number& n) {
  return init_inc(incpos(n, n.size()-1));
}

bool hasRepetition(const number& n) {
  return std::adjacent_find(n.cbegin(), n.cend()) != n.cend();
}

bool hasExactRepetition(const number& n, const int replength = 2) {
  for (auto adj_end = n.begin(); adj_end != n.end();) {
    auto adj_begin = std::adjacent_find(adj_end, n.end());
    if (adj_begin == n.end()) return false;
    const Digit adj_val = *adj_begin;
    adj_end = std::find_if(std::next(adj_begin), n.end(),
                           [adj_val](const Digit d)
                           { return d != adj_val; });
    if (std::distance(adj_begin, adj_end) == replength) return true;
  }
}

int main() {
  const number first = {1,0,9,1,6,5};
  const number last  = {5,7,6,7,2,3};

  print(first);

  int answer1 = 0;
  int answer2 = 0;

  number n = init_inc(first);

  for (number n = init_inc(first); n <= last; n=inc(n)) {
    answer1 += hasRepetition(n);
    answer2 += hasExactRepetition(n);
  }

  std::cout << "Answer 1: " << answer1 << std::endl;
  std::cout << "Answer 2: " << answer2 << std::endl;

  return 0;
}
