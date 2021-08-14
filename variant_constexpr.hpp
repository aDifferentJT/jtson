#ifndef variant_constexpr_hpp
#define variant_constexpr_hpp

#include <algorithm>
#include <concepts>
#include <memory>
#include <type_traits>

// std::variant gains constexpr destructor in a defect report for C++20
// But I don't really like the design anyway, most notably of visit

namespace variant_impl {
  template <typename T> struct tag {};

  template <std::size_t I, typename T, typename CRTP>
  struct helper {
    constexpr auto get_index(tag<T>) const -> std::size_t { return I; }

    constexpr auto holds(tag<T>) const -> bool {
      auto _this = static_cast<CRTP const *>(this);
      return _this->_index == I;
    }

    constexpr auto get_if(tag<T>) -> T* {
      auto _this = static_cast<CRTP*>(this);
      if (_this->_index == I) {
        return &_this->data.template get<T>();
      } else {
        return nullptr;
      }
    }

    constexpr auto get_if(tag<T>) const -> T const * {
      auto _this = static_cast<CRTP const *>(this);
      if (_this->_index == I) {
        return &_this->data.template get<T>();
      } else {
        return nullptr;
      }
    }
  };

  template <typename ...>
  union variant_union {};

  template <typename T, typename ...Ts>
  union variant_union<T, Ts...> {
    private:
      T first;
      variant_union<Ts...> rest;
  
      constexpr variant_union(std::bool_constant<true>, auto&& ...args) : first{std::forward<decltype(args)>(args)...} {}
      constexpr variant_union(std::bool_constant<false>, auto&& ...args) : rest{std::forward<decltype(args)>(args)...} {};

    public:
      template <typename U>
      constexpr variant_union(U&& x) : variant_union{std::bool_constant<std::is_same_v<T, std::decay_t<U>>>{}, std::forward<decltype(x)>(x)} {}

      constexpr ~variant_union() {}
  
      template <typename U>
      constexpr auto get() & -> U& {
        if constexpr (std::is_same_v<T, U>) {
          return first;
        } else {
          return rest.template get<U>();
        }
      }
  
      template <typename U>
      constexpr auto get() const & -> U const & {
        if constexpr (std::is_same_v<T, U>) {
          return first;
        } else {
          return rest.template get<U>();
        }
      }
  
      template <typename U>
      constexpr auto get() && -> U&& {
        if constexpr (std::is_same_v<T, U>) {
          return std::move(first);
        } else {
          return std::move(rest).template get<U>();
        }
      }
  };
}

template <typename Is, typename ...Ts>
class variant_constexpr_indexed;

template <typename ...Ts>
using variant_constexpr = variant_constexpr_indexed<std::index_sequence_for<Ts...>, Ts...>;

template <std::size_t ...Is, typename ...Ts>
class variant_constexpr_indexed<std::index_sequence<Is...>, Ts...> : private variant_impl::helper<Is, Ts, variant_constexpr<Ts...>>... {
  private:
    union {
      variant_impl::variant_union<Ts...> data;
    };
    std::size_t _index;

    template <std::size_t I, typename T, typename CRTP>
    friend struct variant_impl::helper;

    using variant_impl::helper<Is, Ts, variant_constexpr_indexed>::get_index...;
    using variant_impl::helper<Is, Ts, variant_constexpr_indexed>::holds...;
    using variant_impl::helper<Is, Ts, variant_constexpr_indexed>::get_if...;

  public:
    template <typename T>
    static constexpr bool is_of_variant = (std::same_as<std::decay_t<T>, Ts> || ... || false);

    constexpr variant_constexpr_indexed(auto&& x) requires is_of_variant<decltype(x)>
      : data{std::forward<decltype(x)>(x)}
      , _index{get_index(variant_impl::tag<std::decay_t<decltype(x)>>{})}
      {}

    constexpr auto index() const { return _index; }

    template <typename T> constexpr auto holds() const { return holds(variant_impl::tag<T>{}); }

    template <typename T> constexpr auto get_if()       { return get_if(variant_impl::tag<T>{}); }
    template <typename T> constexpr auto get_if() const { return get_if(variant_impl::tag<T>{}); }

    template <typename Ret>
    constexpr auto visit(auto const & f) & -> Ret {
      constexpr auto fs = std::array
        {+[](decltype(f) f, decltype((data)) data) -> Ret { return f(data.template get<Ts>()); }...};
      return fs[_index](f, data);
    }

    template <typename Ret>
    constexpr auto visit(auto const & f) const & -> Ret {
      constexpr auto fs = std::array
        {+[](decltype(f) f, decltype((data)) data) -> Ret { return f(data.template get<Ts>()); }...};
      return fs[_index](f, data);
    }

    template <typename Ret>
    constexpr auto visit(auto const & f) && -> Ret {
      constexpr auto fs = std::array
        {+[](decltype(f) f, decltype(std::move(data)) data) -> Ret { return f(std::move(data).template get<Ts>()); }...};
      return fs[_index](f, std::move(data));
    }

    template <typename Ret>
    constexpr auto match(auto const & ...fs) & -> Ret {
      constexpr auto gs = std::array
        {+[](std::tuple<decltype(fs)...> fs, decltype((data)) data) -> Ret { return std::get<Is>(fs)(data.template get<Ts>()); }...};
      return gs[_index](std::tuple{fs...}, data);
    }

    template <typename Ret>
    constexpr auto match(auto const & ...fs) const & -> Ret {
      constexpr auto gs = std::array
        {+[](std::tuple<decltype(fs)...> fs, decltype((data)) data) -> Ret { return std::get<Is>(fs)(data.template get<Ts>()); }...};
      return gs[_index](std::tuple{fs...}, data);
    }

    template <typename Ret>
    constexpr auto match(auto const & ...fs) && -> Ret {
      constexpr auto gs = std::array
        {+[](std::tuple<decltype(fs)...> fs, decltype(std::move(data)) data) -> Ret { return std::get<Is>(fs)(std::move(data).template get<Ts>()); }...};
      return gs[_index](std::tuple{fs...}, std::move(data));
    }

    constexpr variant_constexpr_indexed(variant_constexpr_indexed const & that) noexcept((std::is_nothrow_copy_constructible_v<Ts> && ... && true)) : _index{that._index} {
      that.template visit<void>([this] <typename T> (T const & x) { std::construct_at(&data, x); });
    }

    constexpr variant_constexpr_indexed(variant_constexpr_indexed&& that) noexcept((std::is_nothrow_move_constructible_v<Ts> && ... && true)) : _index{that._index} {
      std::move(that).template visit<void>([this] <typename T> (T&& x) { std::construct_at(&data, std::move(x)); });
    }

    constexpr variant_constexpr_indexed& operator=(variant_constexpr_indexed const & that) {
      if (&that != this) {
        visit<void>([](auto& x) { std::destroy_at(&x); });
        std::destroy_at(&data);
        that.template visit<void>([this] <typename T> (T const & x) { std::construct_at(&data, x); });
      }
    }

    constexpr variant_constexpr_indexed& operator=(variant_constexpr_indexed&& that) {
      visit<void>([](auto& x) { std::destroy_at(&x); });
      std::destroy_at(&data);
      std::move(that).template visit<void>([this] <typename T> (T&& x) { std::construct_at(&data, std::move(x)); });
    }

    constexpr ~variant_constexpr_indexed() {
      visit<void>([](auto& x) { std::destroy_at(&x); });
    }
};

#endif
