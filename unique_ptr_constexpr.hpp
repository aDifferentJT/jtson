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

    constexpr unique_ptr_constexpr(unique_ptr_constexpr&& that) : data{std::exchange(that.data, nullptr)} {}

    constexpr unique_ptr_constexpr& operator=(unique_ptr_constexpr&& that) {
      auto tmp = std::move(that);
      swap(*this, tmp);
      return *this;
    }

    constexpr ~unique_ptr_constexpr() { delete data; }

    constexpr operator bool() const { return data; }

    constexpr auto get()       -> T       * { return data; }
    constexpr auto get() const -> T const * { return data; }

    constexpr auto operator*()       -> T       & { return *data; }
    constexpr auto operator*() const -> T const & { return *data; }

    constexpr auto operator->()       -> T       * { return data; }
    constexpr auto operator->() const -> T const * { return data; }

    template <typename>
    friend constexpr auto make_unique_constexpr(auto&& ...);
};

template <typename T>
constexpr auto make_unique_constexpr(auto&& ...args) { return unique_ptr_constexpr<T>{new T{std::forward<decltype(args)>(args)...}}; }

#endif
