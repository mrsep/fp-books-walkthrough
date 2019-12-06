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

enum class OPCODE {
  add  = 1,
  mul  = 2,
  in  = 3,
  out = 4,
  halt = 99
};

enum class ACCESS_MODE {
  position  = 0,
  immediate = 1
};

enum class RESULT {
  normal,
  halt,
  error
};

Int op_fetch(const Mem& mem, const Int ip) {
  return mem.at(ip);
}

std::vector<Int> data_fetch(const Mem& mem, const Int ip, const std::vector<ACCESS_MODE>& modes) {
  std::vector<Int> result; result.reserve(modes.size());
  for (std::size_t i=0; i<modes.size(); ++i) {
    const ACCESS_MODE m = modes[i];
    if (m == ACCESS_MODE::position)
      result.push_back(mem.at(mem.at(ip+1+i)));
    else if (m == ACCESS_MODE::immediate)
      result.push_back(mem.at(ip+1+i));
    else
      std::cerr << "Unknown access mode!\n";
  }

  return result;
}

Int numPara(const OPCODE opcode) {
  if (opcode == OPCODE::add || opcode == OPCODE::mul) return 3l;
  if (opcode == OPCODE::in  || opcode == OPCODE::out) return 1l;
  if (opcode == OPCODE::halt)                         return 0l;
  else                                                return 0l;
}

std::pair<OPCODE, std::vector<ACCESS_MODE>> decode(Int code) {
  const OPCODE opcode = static_cast<OPCODE>(code % Int{100});
  const Int num_para = numPara(opcode);
  std::vector<ACCESS_MODE> modes(num_para, ACCESS_MODE::position);

  code /= Int{100};
  for (std::size_t pos=0; (code != Int{0}) && pos < modes.size(); ++pos) {
    modes[pos] = static_cast<ACCESS_MODE>( code % Int{10} );
    code /= Int{10};
  }

  return {opcode, modes};
}

void data_store(Mem& mem, const Int pos, Int val) {
  mem[mem[pos]] = val;
}

void read(Mem& mem, Int pos) {
  Int val;
  std::cin >> val;
  data_store(mem, pos, val);
}

void write(Int val) {
  std::cout << val;
}

RESULT execute(Mem& mem, const OPCODE opcode, const std::vector<Int>& data) {
       if (opcode == OPCODE::add ) { data_store(mem, data.back(), data[0] + data[1]); return RESULT::normal; }
  else if (opcode == OPCODE::mul ) { data_store(mem, data.back(), data[0] * data[1]); return RESULT::normal; }
  else if (opcode == OPCODE::in  ) { read(mem, data.front());                         return RESULT::normal; }
  else if (opcode == OPCODE::out ) { write(data.front());                             return RESULT::normal; }
  else if (opcode == OPCODE::halt) { return RESULT::halt;   }
  else                             { return RESULT::error;  }
}

bool run(Mem& mem, Int ip_) {
  OPCODE op = OPCODE::halt;
  std::vector<ACCESS_MODE> modes;

  for (Int num_para = 0, ip = ip_; ; ip += 1+num_para) {
    std::tie(op,modes) = decode(op_fetch(mem, ip));
    num_para = numPara(op);

    const RESULT result = execute(mem, op, data_fetch(mem, ip, modes));
    if (result == RESULT::normal) continue;
    if (result == RESULT::halt  ) return true;
    if (result == RESULT::error ) return false;
  }
  return true;
}

void answer1(const Disk& disk) {
  Mem mem = convert(disk);

  if (!run(mem, Int{0})) {
    std::cerr << "something went wrong!" << std::endl;
  }
}

int main() {
  const Disk disk = readData("../aoc05.txt");
  for (const auto i : disk) { std::cout << i << std::endl; }

  std::cout << "Answer 1: "; answer1(disk); std::cout << std::endl;

  return 0;
}

