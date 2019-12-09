#include <cmath>
#include <vector>
#include <algorithm>
#include <numeric>
#include <iostream>
#include <iterator>
#include <fstream>
#include <iomanip>

using mass_t = long long;
using fuel_t = long long;

std::vector<mass_t> readData(const std::string filename) {
  std::vector<mass_t> result;

  std::ifstream ifs{filename};
  if (ifs) {
    std::copy(std::istream_iterator<mass_t>(ifs),
              std::istream_iterator<mass_t>(),
              std::back_inserter(result));
  }

  return result;
}

fuel_t computeFuel(mass_t mass) {
  return fuel_t( std::floor(mass / 3.0L) - 2.0L );
}

fuel_t computeFuelFuel(mass_t mass, fuel_t total_fuel) {
  const fuel_t fuel = computeFuel(mass);

  if (fuel <= fuel_t{}) return total_fuel;
  else                  return computeFuelFuel(fuel, total_fuel + fuel);
}

fuel_t computeTotalFuel(mass_t mass) {
  return computeFuelFuel(mass, fuel_t{});
}
