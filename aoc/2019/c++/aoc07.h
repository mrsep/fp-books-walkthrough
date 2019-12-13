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

template <typename I = long>
class IntCode {
public:
  using Int  = I;
  using Disk = std::vector<Int>;
  using Mem  = std::map<Int,Int>;

private:
  Mem mem;

public:
  std::vector<Int> input;
  std::vector<Int> output;
  Int input_index;

public:
  IntCode(const Disk& disk)
    : IntCode(disk, {}) {}

  IntCode(const Disk& disk, const std::vector<Int>& input)
    : mem(convert(disk))
    , input(input)
    , input_index(0)
    , output() {}

  static Disk readData(const std::string filename) {
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

  static Mem convert(const Disk& input) {
    Mem result;
    for (int i=0; i < input.size(); i++) { result[i] = input[i]; }
    return result;
  }

  enum class OPCODE {
    add  =  1,
    mul  =  2,
    in   =  3,
    out  =  4,
    jt   =  5,
    jf   =  6,
    lt   =  7,
    eq   =  8,
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

  Int op_fetch(const Int ip) const {
    return mem.at(ip);
  }

  std::vector<Int> data_fetch(const Int ip, const std::vector<ACCESS_MODE>& modes) const {
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

  static Int numPara(const OPCODE opcode) {
    if (opcode == OPCODE::add || opcode == OPCODE::mul) return 3l;
    if (opcode == OPCODE::lt  || opcode == OPCODE::eq ) return 3l;
    if (opcode == OPCODE::jt  || opcode == OPCODE::jf ) return 2l;
    if (opcode == OPCODE::in  || opcode == OPCODE::out) return 1l;
    if (opcode == OPCODE::halt)                         return 0l;
    else                                                return 0l;
  }

  static std::pair<OPCODE, std::vector<ACCESS_MODE>> decode(Int code) {
    const OPCODE opcode = static_cast<OPCODE>(code % Int{100});
    const Int num_para = numPara(opcode);
    std::vector<ACCESS_MODE> modes(num_para, ACCESS_MODE::position);

    code /= Int{100};
    for (std::size_t pos=0; (code != Int{0}) && pos < modes.size(); ++pos) {
      modes[pos] = static_cast<ACCESS_MODE>( code % Int{10} );
      code /= Int{10};
    }

    if (opcode == OPCODE::add ||
        opcode == OPCODE::mul ||
        opcode == OPCODE::in  ||
        opcode == OPCODE::lt  ||
        opcode == OPCODE::eq)
    {
      modes.back() = ACCESS_MODE::immediate;
    }

    return {opcode, modes};
  }

  void data_store(const Int pos, Int val) {
    mem[pos] = val;
  }

  void read(Int pos) {
    const Int val = input[input_index++];
    data_store(pos, val);
  }

  void write(Int val) {
    output.push_back(val);
  }

  static void fwd(Int& ip, const OPCODE op) {
    ip += 1+numPara(op);
  }

  static Int get(Int ip, const OPCODE op) {
    return ip + 1 + numPara(op);
  }

  RESULT execute(Int& ip, const OPCODE op, const std::vector<Int>& data) {
         if (op == OPCODE::add ) { fwd(ip,op); data_store(data.back(), data[0] + data[1]);       return RESULT::normal; }
    else if (op == OPCODE::mul ) { fwd(ip,op); data_store(data.back(), data[0] * data[1]);       return RESULT::normal; }
    else if (op == OPCODE::in  ) { fwd(ip,op); read(data.front());                               return RESULT::normal; }
    else if (op == OPCODE::out ) { fwd(ip,op); write(data.front());                              return RESULT::normal; }
    else if (op == OPCODE::jt  ) { ip = data.front() != 0l ? data.back() : get(ip,op);           return RESULT::normal; }
    else if (op == OPCODE::jf  ) { ip = data.front() == 0l ? data.back() : get(ip,op);           return RESULT::normal; }
    else if (op == OPCODE::lt  ) { fwd(ip,op); data_store(data.back(), Int(data[0]  < data[1])); return RESULT::normal; }
    else if (op == OPCODE::eq  ) { fwd(ip,op); data_store(data.back(), Int(data[0] == data[1])); return RESULT::normal; }
    else if (op == OPCODE::halt) { return RESULT::halt;   }
    else                         { return RESULT::error;  }
  }

  bool run(Int ip_) {
    OPCODE op = OPCODE::halt;
    std::vector<ACCESS_MODE> modes;

    for (Int ip = ip_; ;) {
      std::tie(op,modes) = decode(op_fetch(ip));

      const RESULT result = execute(ip, op, data_fetch(ip, modes));
      if (result == RESULT::normal) continue;
      if (result == RESULT::halt  ) return true;
      if (result == RESULT::error ) return false;
    }
    return true;
  }
};

using IC = IntCode<long>;
using Int = IC::Int;

Int answer1() {
  Int best = 0;
  Int prev = 0;

  std::vector<Int> perm{0,1,2,3,4};
  do {

    for (const Int phase : perm) {
      const std::vector<Int> input = {phase, prev};
      IC ic(IC::readData("../aoc07.txt"), input);
      ic.run(Int{});
      prev = ic.output.back();
    }

    best = std::max(best, prev);
    prev = 0;
  } while (std::next_permutation(perm.begin(), perm.end()));

  return best;
}
