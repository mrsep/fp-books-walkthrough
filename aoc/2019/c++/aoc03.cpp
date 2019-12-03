#include <cmath>
#include <vector>
#include <map>
#include <algorithm>
#include <numeric>
#include <iostream>
#include <iterator>
#include <fstream>
#include <iomanip>
#include <Eigen/Core>

using Int  = int;
using Point= Eigen::Vector2i;

using Data = std::pair<char,Int>;
using Disk = std::vector<Data>;

using Direction = std::pair<Point,Int>;
using Path      = std::vector<Direction>;

using Wire = std::vector<Point>;

Disk readData(const std::string filename) {
  Disk result;

  std::ifstream ifs{filename};
  if (ifs) {
    std::string token;
    while(std::getline(ifs, token, ',')) {
      result.push_back({token.front(), std::stol(token.substr(1))});
    }
  }

  return result;
}

Direction convert(const Data& data) {
       if (data.first == 'R') return {Point( 1, 0), data.second};
  else if (data.first == 'U') return {Point( 0, 1), data.second};
  else if (data.first == 'L') return {Point(-1, 0), data.second};
  else if (data.first == 'D') return {Point( 0,-1), data.second};
  else {
    std::cerr << "Wong direction character!" << std::endl;
    return {};
  }
}

Path convert(const Disk& input) {
  Path result;
  for (int i=0; i < input.size(); i++) { 
    result.push_back(convert(input[i])); 
  }
  return result;
}

Wire convert(const Direction& dir, const Point& start) {
  Wire result; result.reserve(dir.second);
  for (Int i=1; i <= dir.second; i++) {
    result.push_back(start + i*dir.first);
  }
  return result;
}

Wire convert(const Path& path, const Point& start) {
  Wire result; result.push_back(start);
  for (const Direction& dir : path) {
    const auto new_wire = convert(dir, result.back());
    std::move(new_wire.begin(), new_wire.end(), std::back_inserter(result));
  }

  return result;
}

bool compare(const Eigen::Vector2i& a, const Eigen::Vector2i& b) {
	return std::make_pair(a[0], a[1]) < std::make_pair(b[0], b[1]);
}

Int norm(const Point& p) {
  return std::abs(p[0]) + std::abs(p[1]);
}

bool compareNorm(const Eigen::Vector2i& a, const Eigen::Vector2i& b) {
  return norm(a) < norm(b);
}

int main() {
  //for (const auto i : disk) { std::cout << i << std::endl; }
  const Disk a = readData("../aoc03a.txt");
  const Disk b = readData("../aoc03b.txt");

  const Path path_a = convert(a);
  const Path path_b = convert(b);

  Wire wire_a = convert(path_a, Eigen::Vector2i::Zero());
  Wire wire_b = convert(path_b, Eigen::Vector2i::Zero());

  std::sort(wire_a.begin(), wire_a.end(), compare);
  std::sort(wire_b.begin(), wire_b.end(), compare);

  Wire inter;
  std::set_intersection(wire_a.cbegin(), wire_a.cend(),
			wire_b.cbegin(), wire_b.cend(),
			std::back_inserter(inter), compare);

  std::sort(inter.begin(), inter.end(), compareNorm);

  const auto firstpos = std::find_if(inter.cbegin(), inter.cend(), [](const Point& p) { return norm(p) > 0; });

  std::cout << "Answer 1: " << norm(*firstpos) << std::endl;

  return 0;
}


