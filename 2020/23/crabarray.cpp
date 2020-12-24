#include <iostream>
#include <ranges>
#include <cstddef>
#include <cstdlib>
#include <stdexcept>
#include <string_view>
#include <utility>
#include <vector>

class crab_array {
  public:
    struct node {
        std::size_t cw;
        std::size_t ccw;

        operator<=>(const node& other) const noexcept = default;

        operator bool() const noexcept
        {
            return *this != missing_node;
        }
    };

    constexpr static node missing_node = { 0, 0, };

    template<class R>
    crab_array(R&& values, std::size_t max) noexcept
        : buf_(max + 1, missing_node), focus_(0)
    {
        std::size_t highest_seen = 0;
        auto it_end = end(values);
        std::size_t last = 0;
        for (auto it = begin(values); it != it_end; ++it) {
            std::size_t this_node = *it;
            buf_[this_node] = { 0, last };
            if (!focus_)
                focus_ = this_node;
            if (last)
                buf_[last].cw = this_node;
            if (this_node > highest_seen)
                highest_seen = this_node;
            last = this_node;
        }

        for (std::size_t i = highest_seen + 1; i < max; ++i) {
            buf_[i] = { 0, last };
            if (!focus_)
                focus_ = i;
            buf_[last].cw = i;
            last = i;
        }

        if (last) {
            buf_[last].cw = focus_;
            buf_[focus_].ccw = last;
        }
    }

    std::size_t max() const noexcept
    {
        return buf_.size() - 1;
    }

    std::size_t focus() const noexcept
    {
        return focus_;
    }

    void move_cw() noexcept
    {
        if (focus_)
            focus_ = buf_[focus_].cw;
    }

    std::size_t insert_cw(std::size_t dest, std::size_t value)
    {
        if (buf_[value])
            throw std::runtime_error("duplicate insert!");
        buf_[value].cw = buf_[dest].cw;
        buf_[dest].cw = value;
        buf_[value].ccw = dest;
        return value;
    }

    std::size_t remove_cw()
    {
        if (!focus_)
            throw std::logic_error("remove_cw from empty array");
        std::size_t ret = buf_[focus_].cw;
        buf_[focus_].cw = buf_[ret].cw;
        buf_[buf_[focus_].cw].ccw = focus_;
        buf_[ret] = missing_node;
        return ret;
    }

    std::vector<std::size_t> to_vector() const
    {
        std::vector<std::size_t> result;
        if (!focus_)
            return result;
        std::size_t cur = focus_;
        do {
            result.push_back(cur);
            cur = buf_[cur].cw;
        } while (cur != focus_);
        return result;
    }

  private:
    std::vector<node> buf_;
    std::size_t focus_;
};

void crab_game(crab_array& cups)
{
    auto first = cups.remove_cw();
    auto second = cups.remove_cw();
    auto third = cups.remove_cw();

    auto seek = cups.focus() - 1;
    if (seek < 1)
        seek = cups.max();
    while (seek == first || seek == second || seek == third) {
        --seek;
        if (seek < 1)
            seek = cups.max();
    }

    seek = cups.insert_cw(seek, first);
    seek = cups.insert_cw(seek, second);
    seek = cups.insert_cw(seek, third);

    cups.move_cw();
}

int main(int argc, char** argv)
{
    if (argc != 4) {
        std::cerr << "Usage: " << argv[0] << " nums max iters\n";
        return 1;
    }

    std::size_t max = std::strtoul(argv[2], nullptr, 10);
    std::size_t iters = std::strtoul(argv[3], nullptr, 10);

    crab_array cups(std::ranges::views::transform(std::string_view(argv[1]),
      [](char c) { return int(c - '0'); }), max);

    for (std::size_t i = 0; i < iters; ++i) {
        crab_game(cups);
    }

    auto v = cups.to_vector();
    for (const auto& c : v) {
        std::cout << c;
    }
    std::cout << '\n';

    return 0;
}
