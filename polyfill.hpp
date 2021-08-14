#ifndef polyfill_hpp
#define polyfill_hpp

namespace polyfill {
  template <typename From, typename To>
  concept convertible_to =
    std::is_convertible_v<From, To> &&
    requires(std::add_rvalue_reference_t<From> (&f)()) {
      static_cast<To>(f());
    };

  template <typename Container>
  struct back_insert_iterator {
    Container* container;

    constexpr back_insert_iterator(Container& container) : container{&container} {}

    constexpr auto operator=(auto&& x) { container->push_back(std::forward<decltype(x)>(x)); }

    constexpr auto operator*()     -> auto& { return *this; }
    constexpr auto operator++()    -> auto& { return *this; }
    constexpr auto operator++(int) -> auto& { return *this; }
  };
}

#endif
