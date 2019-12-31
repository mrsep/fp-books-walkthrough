#include <cassert>
#include <cmath>
#include <vector>
#include <map>
#include <algorithm>
#include <numeric>
#include <iostream>
#include <iterator>
#include <fstream>
#include <iomanip>

#include <Eigen/Dense>

using Scalar = bool;
using Space = Eigen::Matrix<Scalar, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>;
using Coord = Eigen::Vector2i;

using Disk = std::vector<std::string>;

Disk readData(const std::string filename) {
  Disk result;

  std::ifstream ifs{filename};
  if (ifs) {
    std::string line;
    while(std::getline(ifs, line)) {
      result.push_back(line);
    }
  }

  return result;
}

bool isInSpace(const Eigen::Vector2i& pos, const Space& space) {
  return 0 <= pos(0) &&
         0 <= pos(1) &&
    pos(0) < space.rows() &&
    pos(1) < space.cols();
}

Space disk2space(const Disk& disk) {
  const std::size_t height = disk.size();
  const std::size_t width  = std::accumulate(disk.cbegin(), disk.cend(),
                                             std::numeric_limits<std::size_t>::max(),
                                             [](const std::size_t acc, const std::string& cur)
                                             { return std::min(acc, cur.size()); });
  Space space{height, width};

  for (std::size_t i=0; i < height; i++) {
    const std::string& line = disk[i];
    for (std::size_t j=0; j < width; j++) {
      space(i,j) = line[j] == '#';
    }
  }

  return space;
}

std::vector<Coord> asteroidsInSpace(const Space& space) {
  std::vector<Coord> result;
  for (int i=0; i<space.rows(); i++) {
    for (int j=0; j<space.cols(); j++) {
      if (space.coeff(i,j)) {
        result.push_back({i,j});
      }
    }
  }

  return result;
}

Eigen::Vector2i reduce(const Eigen::Vector2i coord) {
  return coord / std::gcd(coord(0), coord(1));
}

bool isDetectable(const Coord& a, const Coord& q, const Space& space) {
  const auto diff = (q - a);
  const auto red = reduce(diff);
  Coord pos = a + red;

  while (isInSpace(pos, space)) {
    if (space(pos(0),pos(1))) return pos == q;
    pos += red;
  }

  return false;
}

std::pair<Coord,int> answer1() {
  const Disk disk = readData("../aoc10.txt");
  const Space space = disk2space(disk);

  const auto asteroids = asteroidsInSpace(space);

  std::vector<std::vector<Coord>> detections{asteroids.size(), std::vector<Coord>{}};
  for (const auto& a : asteroids) {
    std::vector<Coord> a_detect;
    for (const auto& q : asteroids) {
      if (a != q && isDetectable(a, q, space)) {
        a_detect.push_back(q);
      }
    }
    detections.push_back(std::move(a_detect));
  }

  const int best_index =
      std::distance(detections.cbegin(),
                    std::max_element(detections.cbegin(), detections.cend(),
                                     [](const std::vector<Coord> &a,
                                        const std::vector<Coord> &b)
                                     { return a.size() < b.size(); }));

  return {asteroids[best_index], detections[best_index].size()};
}
