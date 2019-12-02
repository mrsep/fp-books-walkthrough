#include <cmath>
#include <vector>
#include <map>
#include <algorithm>
#include <numeric>
#include <iostream>
#include <iterator>
#include <fstream>
#include <iomanip>

using Int = long;

std::vector<Int> readData(const std::string filename) {
  std::vector<Int> result;

  std::ifstream ifs{filename};
  if (ifs) {
    std::string token;
    while(std::getline(ifs, token, ',')) {
      result.push_back(std::stol(token));
    }
  }

  return result;
}

std::map<Int,Int> convert(const std::vector<Int>& input) {
  std::map<Int,Int> result;
  for (int i=0; i < input.size(); i++) { result[i] = input[i]; }
  return result;
}

bool execute(std::map<Int,Int>& mem, Int pc_) {
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

int main() {
  const auto input = readData("../aoc02.txt");
  //for (const auto i : input) { std::cout << i << std::endl; }
  auto mem = convert(input);

  mem[1] = 12;
  mem[2] =  2;

  if (!execute(mem, Int{})) {
    std::cerr << "something went wrong!" << std::endl;
  }
  std::cout << "Answer 1: " << mem[0] << std::endl;
  //std::cout << "Answer 2: " << 100*mem[1] + mem[2];

  const std::vector<Int> test1{ 1,9,10,3, 2,3,11,0, 99, 30,40,50 };

  auto mem1 = convert(test1);

  if (!execute(mem1, Int{})) {
    std::cerr << "something went wrong!" << std::endl;
  }
  //for (const auto i : mem1) { std::cout << i.second << std::endl; }
}
