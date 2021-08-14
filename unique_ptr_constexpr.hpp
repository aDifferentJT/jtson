#ifndef unique_ptr_constexpr_hpp
#define unique_ptr_constexpr_hpp

// std::unique_ptr gets constexpr support in C++23
template <typename T>
class unique_ptr_constexpr {
  private:
    T* data;

    constexpr unique_ptr_constexpr(T* data) : data{data} {}

  public:
    friend constexpr void swap(unique_ptr_constexpr& lhs, unique_ptr_constexpr& rhs) {
      std::swap(lhs.data, rhs.data);
    }

    constexpr unique_ptr_constexpr() : data{nullptr} {}
    constexpr unique_ptr_constexpr(std::nullptr_t) : data{nullptr} {}

    unique_ptr_constexpr(unique_ptr_constexpr const &) = delete;
    unique_ptr_constexpr& operator=(unique_ptr_constexpr const &) = delete;

    constexpr unique_ptr_constexpr(unique_ptr_constexpr&& that) noexcept : data{std::exchange(that.data, nullptr)} {}

    constexpr unique_ptr_constexpr& operator=(unique_ptr_constexpr&& that) noexcept {
      auto tmp = std::move(that);
      swap(*this, tmp);
      return *this;
    }

    constexpr ~unique_ptr_constexpr() { delete data; }

    constexpr operator bool() const { return data; }

    constexpr auto get()       -> T       * { return data; }
    constexpr auto get() const -> T const * { return data; }

    constexpr auto operator*()       &  -> T       &  { return *data; }
    constexpr auto operator*() const &  -> T const &  { return *data; }
    constexpr auto operator*()       && -> T       && { return std::move(*data); }

    constexpr auto operator->()       -> T       * { return data; }
    constexpr auto operator->() const -> T const * { return data; }

    template <typename>
    friend constexpr auto make_unique_constexpr(auto&& ...);

    constexpr auto copy() const -> unique_ptr_constexpr;
};

template <typename T>
constexpr auto make_unique_constexpr(auto&& ...args) { return unique_ptr_constexpr<T>{new T{std::forward<decltype(args)>(args)...}}; }

template <typename T>
constexpr auto unique_ptr_constexpr<T>::copy() const -> unique_ptr_constexpr {
  if (data) {
    return make_unique_constexpr<T>(*data);
  } else {
    return nullptr;
  }
}

#endif
