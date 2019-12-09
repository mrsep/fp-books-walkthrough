#include "aoc01.h"

int main() {

  const std::vector<mass_t> mass = readData("../aoc01.txt");

  std::vector<fuel_t> fuel; fuel.reserve(mass.size());
  std::transform(mass.cbegin(), mass.cend(), std::back_inserter(fuel), &computeTotalFuel);

  const fuel_t total_fuel = std::accumulate(fuel.cbegin(), fuel.cend(), fuel_t{});

  std::cout << "fuel(12)= " << computeTotalFuel(12) << std::endl;
  std::cout << "fuel(14)= " << computeTotalFuel(14) << std::endl;
  std::cout << "fuel(1969)= " << computeTotalFuel(1969) << std::endl;
  std::cout << "fuel(100756)= " << computeTotalFuel(100756) << std::endl;
  std::cout << "total fuel= " << total_fuel << std::endl;
}
