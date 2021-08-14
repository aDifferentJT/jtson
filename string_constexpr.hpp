#ifndef string_constexpr_hpp
#define string_constexpr_hpp

#include <algorithm>
#include <utility>

class string_constexpr {
  private:
    static constexpr char empty[] = "";
    char const * data;
    std::size_t size;

  public:
    friend constexpr void swap(string_constexpr& lhs, string_constexpr& rhs) {
      std::swap(lhs.data, rhs.data);
      std::swap(lhs.size, rhs.size);
    }

    constexpr string_constexpr() : data{empty}, size{0} {}

    constexpr string_constexpr(std::string_view sv)
      : data
        { [&] {
            auto new_data = new char[sv.size() + 1];
            auto it = std::copy_n(sv.data(), sv.size(), new_data);
            *it = '\0';
            return new_data;
          }()
        }
      , size{sv.size()}
      {}

    constexpr auto view() const { return std::string_view{data, size}; }

    constexpr string_constexpr(string_constexpr const & that) : string_constexpr{that.view()} {}

    constexpr string_constexpr(string_constexpr&& that)
      : data{std::exchange(that.data, empty)}
      , size{std::exchange(that.size, 0)}
      {}

    constexpr string_constexpr& operator=(string_constexpr that) {
      swap(*this, that);
      return *this;
    }

    constexpr ~string_constexpr() { if (data != empty) { delete[] data; } }

    constexpr auto c_str() const -> char const * { return data; }

    constexpr operator std::string_view() const { return view(); }

    friend auto operator<<(std::ostream& os, string_constexpr const & str) -> std::ostream& {
      return os << str.view();
    }
};

namespace literals {
  constexpr auto operator""_s_ce(char const * str, std::size_t len) {
    return string_constexpr{{str, len}};
  }
}

#endif
