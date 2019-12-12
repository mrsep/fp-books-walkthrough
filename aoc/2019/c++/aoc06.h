#include <cmath>
#include <vector>
#include <map>
#include <algorithm>
#include <numeric>
#include <iostream>
#include <iterator>
#include <fstream>
#include <iomanip>

using T = long;
using Orbit = std::string;
using OrbitData = std::map<Orbit,T>;

using OrbitMap = std::map<Orbit, std::vector<Orbit>>;
using Disk     = std::vector<std::pair<Orbit,Orbit>>;

Disk readData(const std::string filename) {
  Disk result;

  std::ifstream ifs{filename};
  if (ifs) {
    Orbit parent, child;
    while(std::getline(ifs, parent, ')') && std::getline(ifs, child, '\n')) {
      result.push_back({parent,child});
    }
  }

  return result;
}

OrbitMap disk2map(const Disk& disk) {
  OrbitMap m;

  for (const auto o_o : disk) {
    m[o_o.first].push_back({ o_o.second, T{} });
  }

  return m;
}

void traverse(const OrbitMap& m, OrbitData& data, Orbit o, T val) {
  if (m.count(o)) {
    const T v = val+1;
    for (const Orbit& c : m.at(o)) {
      data[c] = v;
      traverse(m, data, c, v);
    }
  }
}

long answer1(const Orbitmap m) {
  const Orbit COM{ "COM" };

  OrbitData data;

  traverse(m, data, COM, 0);

  return std::accumulate(data.cbegin(), data.cend(), 0l,
                         [](const T s, const std::pair<const Orbit, T>& o_d)
                         { return s + o_d.second; });
}
