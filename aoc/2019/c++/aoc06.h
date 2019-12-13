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
using OrbitPath = std::vector<Orbit>;

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

bool findPath(const OrbitMap& m, const Orbit& query, const Orbit& curr, OrbitPath& path) {
  if (m.count(curr)) {
    const auto& childs = m.at(curr);

    // check if query is one of the childs
    for (const auto& c : childs) {
      if (c == query) {
        return true;
      }
    }

    // search recursively for the query
    for (const auto& c : childs) {
      if (findPath(m, query, c, path)) {
        path.push_back(curr);
        return true;
      }
    }
  }

  return false;
}

long answer1(const OrbitMap& m) {
  const Orbit COM{ "COM" };

  OrbitData data;

  traverse(m, data, COM, 0);

  return std::accumulate(data.cbegin(), data.cend(), 0l,
                         [](const T s, const std::pair<const Orbit, T>& o_d)
                         { return s + o_d.second; });
}

long answer2(const OrbitMap& m) {
  const Orbit COM{ "COM" };
  const Orbit YOU{ "YOU" };
  const Orbit SAN{ "SAN" };

  OrbitPath path_you;
  OrbitPath path_san;

  if (!findPath(m, YOU, COM, path_you)) {
    std::cerr << "Orbit " << YOU << " has not been found!";
    return std::numeric_limits<long>::max();
  }
  if (!findPath(m, SAN, COM, path_san)) {
    std::cerr << "Orbit " << SAN << " has not been found!";
    return std::numeric_limits<long>::max();
  }

  OrbitPath::const_reverse_iterator it_you;
  OrbitPath::const_reverse_iterator it_san;

  if (path_you.size() <= path_san.size()) {
    std::tie(it_you, it_san) = std::mismatch(path_you.crbegin(), path_you.crend(),
                                             path_san.crbegin());
  }
  else {
    std::tie(it_san, it_you) = std::mismatch(path_san.crbegin(), path_san.crend(),
                                             path_you.crbegin());
  }

  return 2 + std::distance(it_you, path_you.crend())
           + std::distance(it_san, path_san.crend());
}
