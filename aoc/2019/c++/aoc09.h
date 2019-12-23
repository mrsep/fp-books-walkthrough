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

template <typename I>
class IntCode {
public:
  using Int     = I;
  //using Address = typename std::make_unsigned<Int>::type;
  using Address = Int;
  using Disk    = std::vector<Int>;
  using Mem     = std::map<Address,Int>;

private:
  Int ip;
  Mem mem;
  Int output;
  Address base_pointer;

public:
  std::vector<Int> input;
  Int input_index;

public:
  IntCode(const Disk& disk)
    : IntCode(disk, {}) {}

  IntCode(const Disk& disk, const std::vector<Int>& input)
    : ip(0)
    , mem(convert(disk))
    , input(input)
    , input_index(0)
    , output(0)
    , base_pointer(0) {}

  void setInput(const std::vector<Int>& in) {
    input       = in;
    input_index = 0;
  }

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
    abp  =  9,
    halt = 99
  };

  enum class ACCESS_MODE {
    position  = 0,
    immediate = 1,
    relative  = 2,
  };

  enum class RESULT {
    normal,
    halt,
    error,
    pause
  };

  Int op_fetch(const Int ip) const {
    return mem.at(ip);
  }

  std::vector<Int> data_fetch(const Int ip, const std::vector<ACCESS_MODE>& modes) {
    std::vector<Int> result; result.reserve(modes.size());
    for (std::size_t i=0; i<modes.size(); ++i) {
      const ACCESS_MODE m = modes[i];
           if (m == ACCESS_MODE::position)
        result.push_back(mem[ip+1+i]);
      else if (m == ACCESS_MODE::immediate)
        result.push_back(ip+1+i);
      else if (m == ACCESS_MODE::relative)
        result.push_back(base_pointer + mem[ip+1+i]);
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
    if (opcode == OPCODE::abp)                          return 1l;
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

    return {opcode, modes};
  }

  void data_store(const Int pos, const Int val) {
    mem[pos] = val;
  }

  void read(const std::vector<Int>& data) {
    const Int val = input[input_index];
    data_store(data.front(), val);
    ++input_index;
  }

  void write(const std::vector<Int>& data) {
    output = mem[data.front()];
  }

  static void fwd(Int& ip, const OPCODE op) {
    ip += 1+numPara(op);
  }

  static Int get(Int ip, const OPCODE op) {
    return ip + 1 + numPara(op);
  }

  void add(const std::vector<Int> &data) {
    data_store(data.back(), mem[data[0]] + mem[data[1]]);
  }

  void mul(const std::vector<Int>& data) {
    data_store(data.back(), mem[data[0]] * mem[data[1]]);
  }

  Int jump_true(const std::vector<Int>& data) {
    return mem[data.front()] != Int{0} ? mem[data.back()] : get(ip, OPCODE::jt);
  }

  Int jump_false(const std::vector<Int>& data) {
    return mem[data.front()] == Int{0} ? mem[data.back()] : get(ip,OPCODE::jf);
  }

  void less_than(const std::vector<Int>& data) {
    data_store(data.back(), Int{mem[data[0]] < mem[data[1]]});
  }

  void equal(const std::vector<Int>& data) {
    data_store(data.back(), Int{mem[data[0]] == mem[data[1]]});
  }

  void set_bp(const std::vector<Int>& data) {
    base_pointer += mem[data.front()];
  }

  RESULT execute(Int& ip, const OPCODE op, const std::vector<Int>& data) {
         if (op == OPCODE::add ) { fwd(ip,op); add(data);       return RESULT::normal; }
    else if (op == OPCODE::mul ) { fwd(ip,op); mul(data);       return RESULT::normal; }
    else if (op == OPCODE::in  ) { fwd(ip,op); read(data);      return RESULT::normal; }
    else if (op == OPCODE::out ) { fwd(ip,op); write(data);     return RESULT::pause ; }
    else if (op == OPCODE::jt  ) { ip = jump_true(data);        return RESULT::normal; }
    else if (op == OPCODE::jf  ) { ip = jump_false(data);       return RESULT::normal; }
    else if (op == OPCODE::lt  ) { fwd(ip,op); less_than(data); return RESULT::normal; }
    else if (op == OPCODE::eq  ) { fwd(ip,op); equal(data);     return RESULT::normal; }
    else if (op == OPCODE::abp ) { fwd(ip,op); set_bp(data);    return RESULT::normal; }
    else if (op == OPCODE::halt) { return RESULT::halt;   }
    else                         { return RESULT::error;  }
  }

  std::pair<RESULT,Int> run() {
    OPCODE op = OPCODE::halt;
    std::vector<ACCESS_MODE> modes;

    while (true) {
      std::tie(op,modes) = decode(op_fetch(ip));

      const RESULT result = execute(ip, op, data_fetch(ip, modes));
      if (result == RESULT::normal) continue;
      return {result, output};
    }
  }
};

using IC = IntCode<long long>;
using Int = IC::Int;

Int answer1() {
  IC ic(IC::readData("../aoc09.txt"), std::vector<Int>{1l});
  // IC ic(std::vector<Int>{1102,34915192,34915192,7,4,7,99,0});
  return ic.run().second;
}

Int answer2() {
  IC ic(IC::readData("../aoc09.txt"), std::vector<Int>{2l});
  return ic.run().second;
}
