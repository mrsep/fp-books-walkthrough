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
    const Int in1    = mem[mem[pc+1]];
    const Int in2    = mem[mem[pc+2]];
    const Int out    = mem[pc+3];

    //std:: cout << "PC= " << pc << " mem= ";
    //for (const auto i : mem) { std::cout << i.second << ','; }
    //std::cout << std::endl;
    if      (opcode ==  1) { mem[out] = in1 + in2; }
    else if (opcode ==  2) { mem[out] = in1 * in2; }
    else if (opcode == 99) { return true;  }
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
  for (Int noun = 0; noun < 1000; noun++) {
    for (Int verb = 0; verb < 1000; verb++) {
      if (answer1(disk, noun, verb) == target) {
        return 100*noun+verb;
      }
    }
  }

  std::cerr << "Answer not found!" << std::endl;

  return -1;
}

int main() {
  const Disk disk = readData("../aoc02.txt");
  //for (const auto i : disk) { std::cout << i << std::endl; }

  std::cout << "Answer 1: " << answer1(disk, 12, 2) << std::endl;
  std::cout << "Answer 2: " << answer2(disk, 19690720) << std::endl;

  return 0;
}

void test() {
  const std::vector<Int> test1{ 1,9,10,3, 2,3,11,0, 99, 30,40,50 };

  auto mem1 = convert(test1);

  if (!execute(mem1, Int{})) {
    std::cerr << "something went wrong!" << std::endl;
  }
  //for (const auto i : mem1) { std::cout << i.second << std::endl; }
}
