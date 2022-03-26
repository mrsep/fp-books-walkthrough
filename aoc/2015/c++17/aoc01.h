#include <cmath>
#include <string>
#include <vector>
#include <algorithm>
#include <numeric>
#include <iostream>
#include <iterator>
#include <fstream>
#include <iomanip>

using Data = std::string;

Data readData(const std::string filename) {
  Data result;

  std::ifstream ifs{filename};
  if (ifs) {
    std::getline(ifs, result);
  }
  else {
    std::cerr << "ERROR: Failed to open " << filename << std::endl;
  }

  return result;
}


int answer1() {
    const Data data = readData("../day01.txt");

    std::vector<int> tmp; tmp.reserve(data.size());
    std::transform(data.cbegin(), data.cend(), std::back_inserter(tmp),
                   [](const auto& c) {
                     if (c == '(') return  1;
                     if (c == ')') return -1;
                     return 0;
                   });

    return std::accumulate(tmp.cbegin(), tmp.cend(), 0);
}

int answer2() {
    const Data data = readData("../day01.txt");

    std::vector<int> tmp; tmp.reserve(data.size());
    std::transform(data.cbegin(), data.cend(), std::back_inserter(tmp),
                   [](const auto& c) {
                     if (c == '(') return  1;
                     if (c == ')') return -1;
                     return 0;
                   });

    std::vector<int> res; res.reserve(data.size());
    std::partial_sum(tmp.cbegin(), tmp.cend(), std::back_inserter(res));

    return 1+std::distance(res.cbegin(), std::find(res.cbegin(), res.cend(), -1));
}
