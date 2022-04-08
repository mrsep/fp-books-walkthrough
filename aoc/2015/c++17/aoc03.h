#include <cmath>
#include <string>
#include <vector>
#include <algorithm>
#include <numeric>
#include <iostream>
#include <iterator>
#include <fstream>
#include <iomanip>

struct Grid {
  using Data = std::string;

  using Direction  = std::pair<int,int>;
  using Directions = std::vector<Direction>;
  using Coordinate = Direction;
  using GridStore  = std::vector<int>;

  Grid(const std::string filename)
    : data{readData("../day03.txt")}
    , EOG{(int)data.size()}
    , EOI{EOG*EOG*4}
    , dirs{initDirs(data)}
    , grid(EOI,0) {}

  Directions getDirections() const { return dirs; }

  Coordinate math2phys (const Coordinate& mc) const {
    return {mc.first + EOG, mc.second + EOG};
  }

  Coordinate phys2math (const Coordinate& pc) const {
    return {pc.first - EOG, pc.second - EOG};
  }

  Coordinate index2mathcoord(const int index) const {
    return phys2math({index % (2*EOG), index/(2*EOG)});
  }

  int mathcoord2index(const Coordinate& mc) const {
    const Coordinate pc = math2phys(mc);
    return pc.first + (2*EOG)*pc.second;
  }

  static Coordinate move(const Coordinate x0, const Direction& move) {
    return {x0.first + move.first, x0.second + move.second};
  }

  Coordinate move_grid(const Coordinate& x0, const Direction& dir) {
    const Coordinate x1 = move(x0, dir);
    grid[mathcoord2index(x1)]++;
    return x1;
  }

  int num_visited() const {
    return std::count_if(grid.cbegin(), grid.cend(), [](const int& v) { return v > 0; });
  }

private:
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

  static Directions initDirs(const Data& data) {
    Directions dirs;
    dirs.reserve(data.size());

    std::transform(data.cbegin(), data.cend(), std::back_inserter(dirs),
                   [](const auto& c) {
                     if (c == '>') return std::make_pair( 1, 0);
                     if (c == '<') return std::make_pair(-1, 0);
                     if (c == '^') return std::make_pair( 0, 1);
                     if (c == 'v') return std::make_pair( 0,-1);
                     return std::make_pair(0,0);
                   });
    return dirs;
  }

private:
  const Data data;
  const Directions dirs;
  const int EOG;
  const int EOI;

  GridStore grid;
};

int answer1() {
  Grid grid{"../day03.txt"};
  const Grid::Directions dirs = grid.getDirections();

  Grid::Coordinate x{0,0};
  for (const auto& dir : dirs) {
    x = grid.move_grid(x, dir);
  }

  return grid.num_visited();
}

int answer2() {
  Grid grid{"../day03.txt"};
  const Grid::Directions dirs = grid.getDirections();

  Grid::Coordinate xa{0,0};
  Grid::Coordinate xb{0,0};
  grid.move_grid(xa, {0,0});
  grid.move_grid(xb, {0,0});

  for (std::size_t i=0; i<dirs.size(); i+=2) {
    xa = grid.move_grid(xa, dirs[i  ]);
    xb = grid.move_grid(xb, dirs[i+1]);
  }

  return grid.num_visited();
}
