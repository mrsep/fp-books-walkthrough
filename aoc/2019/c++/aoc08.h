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
    std::transform(std::istreambuf_iterator<char>(ifs),
                   std::istreambuf_iterator<char>(),
                   std::back_inserter(result),
                   [](char c) { return c - '0'; });
  }

  return result;
}

template <int width, int height, typename T = Int>
using Layer = Eigen::Array<T, width, height, Eigen::RowMajor>;

template<typename Scalar> struct pixelFold {
  EIGEN_EMPTY_STRUCT_CTOR(pixelFold)
  typedef Scalar result_type;
  Scalar operator()(const Scalar& left, const Scalar& right) const {
    // {0,1},2 -> {0,1}
    // 2,{0,1} -> {0,1}
    // 2,2     -> 2
    // 1,0     -> 1
    // 0,1     -> 0
    const auto minmax = std::minmax(left,right);
    return (minmax.second == 2)
      ? minmax.first
      : left;
  }
};

template <int width, int height, typename T = Int>
struct LayerAdd {
  using Layer_t = Layer<width, height, T>;
  Layer_t operator() (const Layer_t& left, const Layer_t& right) const {
    return left.binaryExpr(right, pixelFold<T>());
  }
};

template <int width, int height, typename T = Int>
struct SpaceImage {
  using Layer_t    = Layer   <width, height, T>;
  using LayerAdd_t = LayerAdd<width, height, T>;

  std::vector<Layer_t> layers;

  static std::size_t numPixels() { return width*height; }
  static SpaceImage read(const Disk& disk);
  static Layer_t transparent;

  Layer_t render() const { return std::accumulate(layers.cbegin(), layers.end(), transparent, LayerAdd_t()); }
};

template <int width, int height, typename T>
Layer<width,height,T> SpaceImage<width, height, T>::transparent = Layer_t::Constant(2);

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

using SI = SpaceImage<6,25>;

int answer1() {
  const Disk disk = readData("../aoc08.txt");
  const auto image = SI::read(disk);

  std::vector<std::size_t> num_zeros; num_zeros.reserve(image.layers.size());
  std::transform(image.layers.cbegin(), image.layers.cend(), std::back_inserter(num_zeros),
                 [](const SI::Layer_t& layer)
                 { return (layer == 0).count(); });

  const SI::Layer_t& layer = image.layers[std::distance(num_zeros.cbegin(),
                                                        std::min_element(num_zeros.cbegin(),
                                                                         num_zeros.cend()))];

  return (layer == 1).count() * (layer == 2).count();
}

std::string answer2() {
  const Disk disk = readData("../aoc08.txt");
  const auto image = SI::read(disk);

  const SI::Layer_t view = image.render();

  std::stringstream ss;
  ss << view;
  return ss.str();
}
