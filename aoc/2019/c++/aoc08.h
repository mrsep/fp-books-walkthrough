#include <vector>
#include <algorithm>
#include <numeric>
#include <Eigen/Dense>

#include <cassert>
#include <cmath>
#include <iostream>
#include <iterator>
#include <fstream>
#include <iomanip>

using Int  = int;
using Disk = std::vector<Int>;

Disk readData(const std::string filename) {
  Disk result;

  std::ifstream ifs{filename};
  if (ifs) {
    std::string token;
    while(std::getline(ifs, token, ',')) {
      result.push_back(std::stol(token));
    }
  }

  return result;
}

template <int width, int height, typename T = Int>
using Layer = Eigen::Matrix<T, width, height, Eigen::RowMajor>;

template <int width, int height, typename T = Int>
struct SpaceImage {
  std::vector<Layer<width, height, T>> layers;

  static std::size_t numPixels() { return width*height; }
  static SpaceImage read(const Disk& disk);
};


template <int width, int height, typename T>
SpaceImage<width, height, T> SpaceImage<width, height, T>::read(const Disk& disk) {
  const auto size = disk.size();
  const auto num_pixels = SpaceImage<width,height, T>::numPixels();
  const auto num_layers = size / num_pixels;

  SpaceImage<width, height, T> result; result.layers.resize(num_layers);

  for (std::size_t l = 0; l < num_layers; ++l) {
    Eigen::Map<const Layer<width, height, T>> view(&( disk.data()[l*num_pixels] ));

    result.layers[l] = view;
  }

  return result;
}

int answer1() {
  const Disk disk = readData("../aoc08.txt");
  const auto image = SpaceImage<25,6>::read(disk);

  std::vector<std::size_t> num_zeros();
  return 0;
}
