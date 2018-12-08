#include <vector>
#include <tuple>
#include <exception>
#include <utility>
#include <iterator>
#include <numeric>
#include <iostream>

struct node {
    std::vector<node> children;
    std::vector<int> metadata;
};

inline std::ostream& operator<<(std::ostream& os, const node& n)
{
    os << "node { children = [";
    for (const auto& c: n.children)
        os << c << ",";
    os << "], metadata = [";
    for (const auto& m: n.metadata)
        os << m << ",";
    return os << "] }";
}

class parse_error: public std::logic_error {
    using std::logic_error::logic_error;
};

template<class It>
inline auto take1(It begin, It end)
{
    if (begin == end)
        throw parse_error("expected input; found end of input");
    auto val = *begin;
    return std::tuple(val, ++begin);
}

template<class It>
std::tuple<node, It> parse_node(It begin, It end)
{
    auto [n_children, in0] = take1(begin, end);
    auto [n_meta, in1] = take1(in0, end);

    node n;
    n.children.reserve(n_children);
    n.metadata.reserve(n_meta);

    auto in = in1;
    for (int i = 0; i < n_children; ++i) {
        auto [child, in1] = parse_node(in, end);
        n.children.emplace_back(std::move(child));
        in = in1;
    }

    for (int i = 0; i < n_meta; ++i) {
        auto [m, in1] = take1(in, end);
        n.metadata.push_back(m);
        in = in1;
    }

    return std::tuple(n, in);
}

int sum(const node& n) noexcept
{
    return std::accumulate(n.metadata.begin(), n.metadata.end(), 0) +
        std::accumulate(n.children.begin(), n.children.end(), 0,
                [](int acc, const node& n) { return acc + sum(n); });
}

int value(const node& n) noexcept
{
    if (n.children.empty())
        return std::accumulate(n.metadata.begin(), n.metadata.end(), 0);
    return std::accumulate(n.metadata.begin(), n.metadata.end(), 0,
        [&](int acc, const int i) {
            auto i_u = static_cast<decltype(n.children.size())>(i);
            if (i_u == 0 || i_u > n.children.size())
                return acc;
            return acc + value(n.children[i_u - 1]);
        });
}

int main()
{
    try {
        auto [node, _] = parse_node(std::istream_iterator<int>(std::cin),
                std::istream_iterator<int>());
        std::cout << sum(node) << '\n';
        std::cout << value(node) << '\n';
    } catch (parse_error& e) {
        std::cerr << e.what() << '\n';
    }
}
