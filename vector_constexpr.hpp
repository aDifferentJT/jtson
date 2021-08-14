#ifndef vector_constexpr_hpp
#define vector_constexpr_hpp

#include <algorithm>
#include <memory>
#include <type_traits>

#include "unique_ptr_constexpr.hpp"

// std::vector gets constexpr support in a DR for C++20
template <typename T, typename Allocator = std::allocator<T>>
class vector_constexpr : private Allocator {
  private:
    T* _data;
    std::size_t _size;
    std::size_t _capacity;

    constexpr void exponential_resize() {
      static_assert(std::is_nothrow_move_constructible_v<T>, "Elements must be nothrow move constructible");
      auto new_capacity = std::max(static_cast<std::size_t>(1), _capacity * 2);
      auto new_data = this->allocate(new_capacity);
      auto it = new_data;
      for (auto& x : *this) {
        std::construct_at(it++, std::move(x));
      }
      if (_data) {
        this->deallocate(_data, _capacity);
      }
      _data = new_data;
      _capacity = new_capacity;
    }

  public:
    using value_type = T;

    constexpr auto operator[](std::size_t i) -> T & {
      if (std::is_constant_evaluated()) {
        if (i < 0 || i >= _size) {
          throw std::out_of_range{""};
        }
      }
      return _data[i];
    }

    constexpr auto operator[](std::size_t i) const -> T const & {
      if (std::is_constant_evaluated()) {
        if (i < 0 || i >= _size) {
          throw std::out_of_range{""};
        }
      }
      return _data[i];
    }

    constexpr auto data()       -> T       * { return _data; }
    constexpr auto data() const -> T const * { return _data; }

    constexpr auto size() const -> std::size_t { return _size; }
    constexpr auto capacity() const -> std::size_t { return _capacity; }

    constexpr auto begin()       -> T       * { return _data; }
    constexpr auto begin() const -> T const * { return _data; }

    constexpr auto end()       -> T       * { return _data + _size; }
    constexpr auto end() const -> T const * { return _data + _size; }

    friend constexpr void swap(vector_constexpr& lhs, vector_constexpr& rhs) {
      std::swap(lhs._data, rhs._data);
      std::swap(lhs._size, rhs._size);
      std::swap(lhs._capacity, rhs._capacity);
      std::swap(static_cast<Allocator&>(lhs), static_cast<Allocator&>(rhs));
    }

    constexpr vector_constexpr(Allocator allocator = {}) : Allocator{std::move(allocator)}, _data{nullptr}, _size{0}, _capacity{0} {}

    constexpr vector_constexpr(std::size_t capacity, Allocator allocator = {}) : Allocator{std::move(allocator)}, _data{this->allocate(capacity)}, _size{0}, _capacity{capacity} {}

    constexpr vector_constexpr(std::initializer_list<T> xs) : vector_constexpr(xs.size()) {
      auto it = _data;
      for (auto& x : xs) {
        std::construct_at(it++, std::move(x));
      }
      _size = xs.size();
    }

    constexpr vector_constexpr(vector_constexpr const & that) : vector_constexpr{that.size(), static_cast<Allocator const &>(that)} {
      auto it = _data;
      for (auto& x : that) {
        std::construct_at(it++, std::move(x));
      }
      _size = that.size();
    }

    constexpr vector_constexpr(vector_constexpr&& that)
      : Allocator{static_cast<Allocator const &>(that)}
      , _data{std::exchange(that._data, nullptr)}
      , _size{std::exchange(that._size, 0)}
      , _capacity{std::exchange(that._capacity, 0)}
      {}

    constexpr vector_constexpr& operator=(vector_constexpr that) {
      swap(*this, that);
      return *this;
    }

    constexpr ~vector_constexpr() {
      if (_data) {
        for (auto& x : *this) {
          std::destroy_at(&x);
        }
        this->deallocate(_data, _capacity);
      }
    }

    constexpr void push_back(T const & x) {
      if (_capacity == _size) {
        exponential_resize();
      }
      std::construct_at(&_data[_size], x);
      _size += 1;
    }

    constexpr void push_back(T&& x) {
      if (_capacity == _size) {
        exponential_resize();
      }
      std::construct_at(&_data[_size], std::move(x));
      _size += 1;
    }

    constexpr void emplace_back(auto&& ...args) {
      if (_capacity == _size) {
        exponential_resize();
      }
      std::construct_at(&_data[_size], std::forward<decltype(args)>(args)...);
      _size += 1;
    }
};

#endif
