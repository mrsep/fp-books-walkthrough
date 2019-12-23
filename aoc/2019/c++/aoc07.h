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
  Int ip;
  Mem mem;
  Int output;

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
    , output(0) {}

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
    halt = 99
  };

  enum class ACCESS_MODE {
    position  = 0,
    immediate = 1
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

  std::vector<Int> data_fetch(const Int ip, const std::vector<ACCESS_MODE>& modes) const {
    std::vector<Int> result; result.reserve(modes.size());
    for (std::size_t i=0; i<modes.size(); ++i) {
      const ACCESS_MODE m = modes[i];
      if (m == ACCESS_MODE::position)
        result.push_back(mem.at(ip+1+i));
      else if (m == ACCESS_MODE::immediate)
        result.push_back(ip+1+i);
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

    return {opcode, modes};
  }

  void data_store(const Int pos, Int val) {
    mem[pos] = val;
  }

  void read(const std::vector<Int>& data) {
    const Int val = input[input_index % 2];
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

  RESULT execute(Int& ip, const OPCODE op, const std::vector<Int>& data) {
         if (op == OPCODE::add ) { fwd(ip,op); add(data);       return RESULT::normal; }
    else if (op == OPCODE::mul ) { fwd(ip,op); mul(data);       return RESULT::normal; }
    else if (op == OPCODE::in  ) { fwd(ip,op); read(data);      return RESULT::normal; }
    else if (op == OPCODE::out ) { fwd(ip,op); write(data);     return RESULT::pause ; }
    else if (op == OPCODE::jt  ) { ip = jump_true(data);        return RESULT::normal; }
    else if (op == OPCODE::jf  ) { ip = jump_false(data);       return RESULT::normal; }
    else if (op == OPCODE::lt  ) { fwd(ip,op); less_than(data); return RESULT::normal; }
    else if (op == OPCODE::eq  ) { fwd(ip,op); equal(data);     return RESULT::normal; }
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

using IC = IntCode<long>;
using Int = IC::Int;

Int answer1() {
  const IC init(IC::readData("../aoc07.txt"));
  Int best = 0;
  Int prev = 0;

  std::vector<Int> perm{0,1,2,3,4};
  do {
    for (const Int phase : perm) {
      IC ic{init};
      ic.setInput(std::vector<Int>{phase, prev});
      prev = ic.run().second;
    }

    best = std::max(best, prev);
    prev = 0;
  } while (std::next_permutation(perm.begin(), perm.end()));

  return best;
}

Int answer2() {
  Int best = 0;
  Int prev = 0;
  IC::RESULT result = IC::RESULT::normal;
  const IC init(IC::readData("../aoc07.txt"));
  //const IC init(IC::Disk{3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5});

  std::vector<Int> perm{5,6,7,8,9};

  do {
    std::vector<IC> ics(5, init);

    for (Int i = 0; ; i++) {
      const Int phase = perm[i % 5];
      IC&          ic =  ics[i % 5];
      ic.setInput((i <= 4)
                  ? std::vector<Int>{phase,prev}
                  : std::vector<Int>{prev});

      std::tie(result,prev) = ic.run();

      best = std::max(best, prev);
      if (result == IC::RESULT::halt) break;
    }

    prev = 0;
  } while (std::next_permutation(perm.begin(), perm.end()));

  return best;
}
