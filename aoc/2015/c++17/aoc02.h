#include <cmath>
#include <string>
#include <vector>
#include <algorithm>
#include <numeric>
#include <iostream>
#include <functional>
#include <iterator>
#include <fstream>
#include <iomanip>

template <typename T>
T min3(const T& a, const T& b, const T&c) {
  return std::min(a, std::min(b, c));
}


struct present {
  int l;
  int w;
  int h;

  long               area() const { return 2l*(l*w + w*h + h*l); }
  long smallest_face_area() const { return min3(l*w, w*h, h*l); }

  long                  volume() const { return l*w*h; }
  long smallest_face_perimeter() const { return 2l*min3(l+w,w+h,l+h); }
};

long wrap_area(const present& p) {
  return p.smallest_face_area() + p.area();
}

long ribbon_length(const present& p) {
  return p.volume() + p.smallest_face_perimeter();
}

using Data = std::vector<present>;

Data readData(const std::string filename) {
  Data result;

  std::ifstream ifs{filename};
  if (ifs) {
    for (std::string line; ifs; ) {
      present p;
      std::getline(ifs, line, 'x');
      if (line.empty()) break;
      p.l = std::stoi(line);
      std::getline(ifs, line, 'x');
      p.w = std::stoi(line);
      std::getline(ifs, line);
      p.h = std::stoi(line);
      result.push_back(p);
    }
  }

  return result;
}


long answer1() {
    const Data data = readData("../day02.txt");

    return std::transform_reduce(data.cbegin(), data.cend(), 0l, std::plus<>(), &wrap_area);
}

long answer2() {
    const Data data = readData("../day02.txt");

    return std::transform_reduce(data.cbegin(), data.cend(), 0l, std::plus<>(), &ribbon_length);
}
