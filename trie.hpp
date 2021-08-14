#ifndef trie_hpp
#define trie_hpp

#include <array>
#include <limits>
#include <optional>
#include <vector>

#include "unique_ptr_constexpr.hpp"

template <typename T>
class trie {
  private:
    template <typename U>
    friend class trie;

    static constexpr auto factor = static_cast<std::size_t>(std::numeric_limits<unsigned char>::max()) + 1;

    // TODO could be std::optional once constexpr optional is available (DR to 20)
    unique_ptr_constexpr<T> here;
    std::array<unique_ptr_constexpr<trie>, factor> nexts;

  public:
    constexpr void swap(trie& lhs, trie& rhs) {
      std::swap(lhs.here, rhs.here);
      std::swap(lhs.nexts, rhs.nexts);
    }

    constexpr trie() = default;

    constexpr trie(trie const & that)
      : here{that.here.copy()}
      , nexts
        { [&] <std::size_t ...Is> (std::index_sequence<Is...>) {
            return std::array{std::get<Is>(that.nexts).copy()...};
          }(std::make_index_sequence<factor>{})
        }
      {}

    constexpr trie& operator=(trie const & that) {
      auto tmp = that;
      swap(*this, tmp);
      return *this;
    }

    // TODO compiler bug? The default version should work
    constexpr trie(trie&& that) noexcept
      : here{std::move(that.here)}
      , nexts
        { [&] <std::size_t ...Is> (std::index_sequence<Is...>) {
            return std::array{std::get<Is>(std::move(that.nexts))...};
          }(std::make_index_sequence<factor>{})
        }
      {}

    constexpr trie& operator=(trie&&) = default;

    template <typename U>
    constexpr trie(auto const & f, trie<U> const & that)
      : here{make_unique_constexpr<T>(f(*that.here))}
      , nexts
        { [&] <std::size_t ...Is> (std::index_sequence<Is...>) {
            auto map = [&](unique_ptr_constexpr<trie<U>> const & x) -> unique_ptr_constexpr<trie<T>> {
              if (x) {
                return make_unique_constexpr<trie>(f, *x);
              } else {
                return nullptr;
              }
            };
            return std::array{map(std::get<Is>(that.nexts))...};
          }(std::make_index_sequence<factor>{})
        }
      {}

    class const_it {
      private:
        std::string key;
        std::vector<trie const *> path;

        void step() {
          auto next = std::find_if(path.back()->nexts.begin(), path.back()->nexts.end(), [](auto const & x) -> auto const & { return x; });
          while (next == path.back()->nexts.end()) {
            auto last = key.back();
            key.pop_back();
            path.pop_back();

            if (path.empty()) {
              return;
            }

            next = std::find_if(path.back()->nexts.begin() + last + 1, path.back()->nexts.end(), [](auto const & x) -> auto const & { return x; });
          }

          key.push_back(next - path.back()->nexts.begin());
          path.push_back(next->get());
        }

        void advance_to_active() {
          while (!path.empty() && !path.back()->here) {
            step();
          }
        };

        const_it(trie const * start) : path{start} {
          advance_to_active();
        }

        const_it() : path{} {}

      public:
        constexpr auto operator*() const {
          return std::pair<std::string, T const &>{key, *path.back()->here};
        }

        constexpr auto operator->() const {
          struct proxy {
            std::pair<std::string, T const &> value;
            constexpr auto operator->() const { return &value; }
          };
          return proxy{**this};
        }

        constexpr auto operator++() -> const_it& {
          step();
          advance_to_active();
          return *this;
        }

        friend constexpr auto operator==(const_it const & lhs, const_it const & rhs) -> bool {
          return (lhs.path.size() == rhs.path.size() && std::equal(lhs.path.begin(), lhs.path.end(), rhs.path.begin()));
        }

        friend class trie;
    };

    constexpr auto begin() const { return const_it{this}; }
    constexpr auto end()   const { return const_it{}; }

    constexpr void emplace(std::string_view key, auto&& ...args) {
      if (key.size() == 0) {
        if (here) { throw 0; /* TODO */ }
        here = make_unique_constexpr<T>(std::forward<decltype(args)>(args)...);
      } else {
        auto& next = nexts[static_cast<unsigned char>(key[0])];
        if (!next) {
          next = make_unique_constexpr<trie>();
        }
        next->emplace(key.substr(1), std::forward<decltype(args)>(args)...);
      }
    }

    constexpr trie(std::initializer_list<std::pair<std::string_view, T>> xs) {
      for (auto& [key, val] : xs) {
        emplace(key, val);
      }
    }

    constexpr auto get_if(std::string_view key) -> T* {
      if (key.size() == 0) {
        if (here) {
          return &*here;
        } else {
          return nullptr;
        }
      } else {
        if (auto& next = nexts[static_cast<unsigned char>(key[0])]) {
          return next->get_if(key.substr(1));
        } else {
          return nullptr;
        }
      }
    }

    constexpr auto get_if(std::string_view key) const -> T const * {
      if (key.size() == 0) {
        if (here) {
          return &*here;
        } else {
          return nullptr;
        }
      } else {
        if (auto& next = nexts[static_cast<unsigned char>(key[0])]) {
          return next->get_if(key.substr(1));
        } else {
          return nullptr;
        }
      }
    }
};

#endif
