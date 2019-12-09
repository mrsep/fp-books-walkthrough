#include <cmath>
#include <vector>
#include <map>
#include <algorithm>
#include <numeric>
#include <iostream>
#include <iterator>
#include <fstream>
#include <iomanip>

using Int  = long;
using Disk = std::vector<Int>;
using Mem  = std::map<Int,Int>;
using MemVec = std::vector<Mem::value_type>;

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

Mem convert(const Disk& input) {
  Mem result;
  for (int i=0; i < input.size(); i++) { result[i] = input[i]; }
  return result;
}

bool execute(Mem& mem, Int pc_) {
  for (Int pc = pc_; ; pc += 4) {
    const Int opcode = mem[pc+0];
    if (opcode == 99) { return true;  }

    const Int in1    = mem[mem[pc+1]];
    const Int in2    = mem[mem[pc+2]];
    const Int out    = mem[pc+3];

    //std:: cout << "PC= " << pc << " mem= ";
    //for (const auto i : mem) { std::cout << i.second << ','; }
    //std::cout << std::endl;
    if      (opcode ==  1) { mem[out] = in1 + in2; }
    else if (opcode ==  2) { mem[out] = in1 * in2; }
    else                   { return false; }
  }
}

Int answer1(const Disk& disk, Int noun, Int verb) {
  Mem mem = convert(disk);

  mem[1] = noun;
  mem[2] = verb;

  if (!execute(mem, Int{})) {
    std::cerr << "something went wrong!" << std::endl;
  }

  return mem[0];
}

Int answer2(const Disk& disk, Int target) {
  for (Int noun = 0; noun < 100; noun++) {
    for (Int verb = 0; verb < 100; verb++) {
      if (answer1(disk, noun, verb) == target) {
        std::cout << "noun= " << noun << " verb= " << verb << std::endl;
        return 100*noun+verb;
      }
    }
  }

  std::cerr << "Answer not found!" << std::endl;

  return -1;
}

MemVec map2vec(const Mem& mem) {
  MemVec result;
  std::copy(mem.cbegin(), mem.cend(), std::back_inserter(result));
  return result;
}
