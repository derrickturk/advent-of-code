#include <tuple>
#include <cstddef>
#include <cstdlib>
#include <cstring>
#include <memory>
#include <limits>
#include <iostream>

template<class T>
constexpr T hundreds(T x) noexcept
{
    return x % 1000 / 100;
}

constexpr int cell_power(int serial, std::size_t x, std::size_t y) noexcept
{
    int rack_id = x + 10;
    return hundreds((rack_id * static_cast<int>(y) + serial) * rack_id) - 5;
}

using coords = std::tuple<std::size_t, std::size_t>;

constexpr coords xy_to_ij(std::size_t x, std::size_t y) noexcept
{
    return std::tuple(y - 1, x - 1);
}

constexpr coords ij_to_xy(std::size_t i, std::size_t j) noexcept
{
    return std::tuple(j + 1, i + 1);
}

class matrix {
  public:
      matrix(std::size_t rows, std::size_t cols)
          : arr_(new int[rows * cols]), rows_(rows), cols_(cols) { }

      matrix(const matrix& other)
          : arr_(new int[other.rows() * other.cols()]),
            rows_(other.rows()), cols_(other.cols())
      {
          std::memcpy(arr_.get(), other.arr_.get(), size() * sizeof(int));
      }

      matrix(matrix&&) noexcept = default;

      template<class F>
      matrix(std::size_t rows, std::size_t cols, F f)
          : arr_(new int[rows * cols]), rows_(rows), cols_(cols)
      {
          for (std::size_t i = 0; i < rows_; ++i)
              for (std::size_t j = 0; j < cols_; ++j)
                  arr_[i * cols_ + j] = f(i, j);
      }

      ~matrix() = default;

      matrix& operator=(const matrix& other)
      {
          matrix new_self(other);
          swap(new_self);
          return *this;
      }

      matrix& operator=(matrix&&) noexcept = default;

      std::size_t rows() const noexcept { return rows_; }
      std::size_t cols() const noexcept { return cols_; }
      std::size_t size() const noexcept { return rows_ * cols_; }

      void swap(matrix& other) noexcept
      {
          using std::swap;
          swap(arr_, other.arr_);
          swap(rows_, other.rows_);
          swap(cols_, other.cols_);
      }

      int& operator[](coords ij) noexcept
      {
          auto [i, j] = ij;
          return arr_[i * cols_ + j];
      }

      const int& operator[](coords ij) const noexcept
      {
          auto [i, j] = ij;
          return arr_[i * cols_ + j];
      }

  private:
      struct row_proxy
      {
          int *ptr_;

          int& operator[](std::size_t j) const noexcept
          {
              return ptr_[j];
          }
      };

      struct const_row_proxy
      {
          const int *ptr_;

          const int& operator[](std::size_t j) const noexcept
          {
              return ptr_[j];
          }
      };

  public:
      row_proxy operator[](std::size_t i) noexcept
      {
          return row_proxy { arr_.get() + i * cols_ };
      }

      const_row_proxy operator[](std::size_t i) const noexcept
      {
          return const_row_proxy { arr_.get() + i * cols_ };
      }

  private:
      std::unique_ptr<int[]> arr_;
      std::size_t rows_, cols_;
};

inline matrix make_grid(std::size_t size, int serial)
{
    return matrix(size, size, [=](auto i, auto j) {
        auto [x, y] = ij_to_xy(i, j);
        return cell_power(serial, x, y);
    });
}

matrix summed_area(const matrix& m)
{
    matrix sum(m);
    for (std::size_t i = 0; i < m.rows(); ++i) {
        for (std::size_t j = 0; j < m.cols(); ++j) {
            if (j > 0)
                sum[i][j] += sum[i][j - 1];
            if (i > 0)
                sum[i][j] += sum[i - 1][j];
            if (i > 0 && j > 0)
                sum[i][j] -= sum[i - 1][j - 1];
        }
    }
    return sum;
}

inline int rectangle_sum(const matrix& sat,
        std::size_t i_min, std::size_t i_max,
        std::size_t j_min, std::size_t j_max) noexcept
{
    int d = sat[i_max][j_max];
    int a = (i_min == 0 || j_min == 0) ? 0 : sat[i_min - 1][j_min - 1];
    int b = (i_min == 0) ? 0 : sat[i_min - 1][j_max];
    int c = (j_min == 0) ? 0 : sat[i_max][j_min - 1];
    return d + a - b - c;
}

inline int cell_square_power(const matrix& grid, const matrix& sat,
        std::size_t x, std::size_t y, std::size_t w) noexcept
{
    auto ij = xy_to_ij(x, y);
    if (w == 1)
        return grid[ij];
    auto [i, j] = ij;
    return rectangle_sum(sat, i, i + w - 1, j, j + w - 1);
}

std::tuple<std::size_t, std::size_t, int> max_power_square_size(
        const matrix& grid, const matrix& sat, std::size_t w) noexcept
{
    std::size_t x_max = 0, y_max = 0;
    int max = std::numeric_limits<int>::min();
    for (std::size_t x = 1; x <= grid.cols() - w + 1; ++x)
        for (std::size_t y = 1; y <= grid.rows() - w + 1; ++y)
            if (int p = cell_square_power(grid, sat, x, y, w); p > max) {
                x_max = x;
                y_max = y;
                max = p;
            }
    return std::tuple(x_max, y_max, max);
}

std::tuple<std::size_t, std::size_t, std::size_t, int> max_power_square(
        const matrix& grid, const matrix& sat) noexcept
{
    std::size_t x_max = 0, y_max = 0, w_max = 0;
    int max = std::numeric_limits<int>::min();
    for (std::size_t w = 1; w <= grid.rows(); ++w)
        if (auto [x, y, p] = max_power_square_size(grid, sat, w); p > max) {
            x_max = x;
            y_max = y;
            w_max = w;
            max = p;
        }
    return std::tuple(x_max, y_max, w_max, max);
}

void print_matrix(const matrix& m)
{
    for (std::size_t i = 0; i < m.rows(); ++i) {
        for (std::size_t j = 0; j < m.cols(); ++j) {
            std::cout << m[i][j] << '\t';
        }
        std::cout << '\n';
    }
}

int main(int argc, char *argv[])
{
    if (argc != 3) {
        std::cerr << "Usage: " << argv[0] << " size serial\n";
        return 0;
    }

    std::size_t size =
        static_cast<std::size_t>(std::strtoul(argv[1], nullptr, 10));
    if (size == 0) {
        std::cerr << "Invalid size \"" << argv[1] << "\"\n";
        return 0;
    }

    int serial = std::atoi(argv[2]);
    if (serial == 0) {
        std::cerr << "Invalid serial \"" << argv[1] << "\"\n";
        return 0;
    }

    const auto grid = make_grid(size, serial);
    const auto sat = summed_area(grid);

    auto [x3, y3, p3] = max_power_square_size(grid, sat, 3);
    std::cout << x3 << ',' << y3 << " - power " << p3 << '\n';

    auto [x, y, w, p] = max_power_square(grid, sat);
    std::cout << x << ',' << y << ',' << w << " - power " << p << '\n';
}
