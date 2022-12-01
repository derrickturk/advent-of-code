#include <algorithm>
#include <array>
#include <cstdlib>
#include <exception>
#include <iostream>
#include <numeric>

template<typename T, std::size_t N>
class biggest {
  private:
    std::array<T, N> _biggest;

  public:
    biggest() noexcept
        : _biggest() { }

    const std::array<T, N>& elements() const noexcept
    {
        return _biggest;
    }

    void witness(T value) noexcept(noexcept(value > value))
    {
        const auto end = std::end(_biggest);
        const auto it = std::find_if(std::begin(_biggest), end,
          [=](auto&& el) { return value > el; });
        std::shift_right(it, end, 1);
        *it = value;
    }
};

int main()
{
    biggest<unsigned long, 3> b;

    unsigned long this_elf = 0;
    std::string line;
    while (std::getline(std::cin, line)) {
        if (line.empty()) {
            b.witness(this_elf);
            this_elf = 0;
            continue;
        }

        try {
            this_elf += std::stoul(line); 
        } catch (const std::exception& e) {
            std::cerr << "Naughty elf: " << e.what() << '\n';
            return 1;
        }
    }

    if (this_elf != 0) {
        b.witness(this_elf);
    }

    std::cout << b.elements()[0] << '\n';
    std::cout <<
      std::reduce(std::begin(b.elements()), std::end(b.elements())) << '\n';
}
