#include <iostream>
#include <ranges>
#include <stdexcept>
#include <string_view>
#include <utility>
#include <vector>

// look, this doesn't have to be great, AoC is a slog
// we're missing some exception safety (e.g. in circular(R&& values))

template<class T>
class circular {
  public:
    struct node {
        T value;
        node* cw;
        node* ccw;
    };

  private:
    node* focus_;
    node* insert_ptr_;

  public:
    circular() noexcept
        : focus_(nullptr) { }

    circular(const circular&) = delete;
    circular(circular&&) = delete;

    template<class R>
    circular(R&& values)
        : focus_(nullptr)
    {
        auto it_end = end(values);
        node* last = nullptr;
        for (auto it = begin(values); it != it_end; ++it) {
            node* this_node = new node(*it, nullptr, last);
            if (!focus_)
                focus_ = this_node;
            if (last)
                last->cw = this_node;
            last = this_node;
        }

        if (last) {
            last->cw = focus_;
            focus_->ccw = last;
        }
    }

    ~circular() noexcept
    {
        node* cur = focus_;
        if (!cur)
            return;
        do {
            node* next = cur->cw;
            delete cur;
            cur = next;
        } while (cur != focus_);
    }

    void move_cw() noexcept
    {
        if (focus_)
            focus_ = focus_->cw;
    }

    T remove_cw()
    {
        if (!focus_)
            throw std::logic_error("remove_cw from empty circular buffer!");
        node* to_pop = focus_->cw;
        focus_->cw = to_pop->cw;
        to_pop->cw->ccw = focus_;
        T ret = std::move(to_pop->value);
        delete to_pop;
        return ret;
    }

    // you gotta pinky swear that dest is derived from the current circular
    const node* insert_cw(const node* dest, T val)
    {
        node* mut_dest = const_cast<node*>(dest);
        node* new_node = new node(std::move(val), mut_dest->cw, mut_dest);
        mut_dest->cw = new_node;
        return new_node;
    }

    const node* focus() const noexcept
    {
        return focus_;
    }

    const T& focus_value() const
    {
        if (!focus_)
            throw std::logic_error("focus_value on empty circular buffer!");
        return focus_->value;
    }

    const node* find(const T& value) const
    {
        if (!focus_)
            throw std::invalid_argument("value not found (empty buffer)!");
        node* cur = focus_;
        do {
            if (cur->value == value)
                return cur;
            cur = cur->cw;
        } while (cur != focus_);
        throw std::invalid_argument("value not found");
    }

    std::vector<T> to_vector() const
    {
        std::vector<T> result;
        if (!focus_)
            return result;
        node* cur = focus_;
        do {
            result.push_back(cur->value);
            cur = cur->cw;
        } while (cur != focus_);
        return result;
    }
};

void crab_game(circular<int>& cups)
{
    auto first = cups.remove_cw();
    auto second = cups.remove_cw();
    auto third = cups.remove_cw();

    auto seek = cups.focus_value() - 1;
    if (seek < 1)
        seek = 9;
    while (seek == first || seek == second || seek == third) {
        --seek;
        if (seek < 1)
            seek = 9;
    }

    auto dest = cups.find(seek);
    dest = cups.insert_cw(dest, first);
    dest = cups.insert_cw(dest, second);
    dest = cups.insert_cw(dest, third);

    cups.move_cw();
}

int main(int argc, char** argv)
{
    if (argc != 2) {
        std::cerr << "Usage: " << argv[0] << " nums\n";
        return 1;
    }

    circular<int> cups(std::ranges::views::transform(std::string_view(argv[1]),
      [](char c) { return int(c - '0'); }));

    for (int i = 0; i < 100; ++i)
        crab_game(cups);

    auto v = cups.to_vector();
    for (const auto& c : v) {
        std::cout << c;
    }
    std::cout << '\n';

    return 0;
}
