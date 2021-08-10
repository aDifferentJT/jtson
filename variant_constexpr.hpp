#ifndef variant_constexpr_hpp
#define variant_constexpr_hpp

#include <algorithm>
#include <concepts>
#include <memory>
#include <type_traits>

// std::variant gains constexpr destructor in a defect report for C++20

namespace variant_impl {
  template <typename T> struct tag {};

  template <std::size_t i, typename T, typename CRTP>
  struct helper {
    constexpr void construct(T const & x) {
      auto _this = static_cast<CRTP*>(this);
      std::construct_at(&_this->data.template get<T>(), x);
      _this->_index = i;
    }

    constexpr void construct(T&& x) {
      auto _this = static_cast<CRTP*>(this);
      std::construct_at(&_this->data.template get<T>(), std::move(x));
      _this->_index = i;
    }

    constexpr auto holds(tag<T>) const -> bool {
      auto _this = static_cast<CRTP const *>(this);
      return _this->_index == i;
    }

    constexpr auto get_if(tag<T>) -> T* {
      auto _this = static_cast<CRTP*>(this);
      if (_this->_index == i) {
        return &_this->data.template get<T>();
      } else {
        return nullptr;
      }
    }

    constexpr auto get_if(tag<T>) const -> T const * {
      auto _this = static_cast<CRTP const *>(this);
      if (_this->_index == i) {
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
    T first;
    variant_union<Ts...> rest;

    constexpr variant_union() {}
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
    variant_impl::variant_union<Ts...> data;
    long _index;

    template <std::size_t I, typename T, typename CRTP>
    friend struct variant_impl::helper;

    using variant_impl::helper<Is, Ts, variant_constexpr_indexed>::construct...;
    using variant_impl::helper<Is, Ts, variant_constexpr_indexed>::holds...;
    using variant_impl::helper<Is, Ts, variant_constexpr_indexed>::get_if...;

  public:
    template <typename T>
    static constexpr bool is_of_variant = (std::same_as<std::decay_t<T>, Ts> || ... || false);

    constexpr variant_constexpr_indexed(auto&& x) requires is_of_variant<decltype(x)> {
      construct(std::forward<decltype(x)>(x));
    }

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

    constexpr variant_constexpr_indexed(variant_constexpr_indexed const & that) : _index{that._index} {
      that.template visit<void>([this] <typename T> (T const & x) { std::construct_at(data.template get<T>(), x); });
    }

    constexpr variant_constexpr_indexed(variant_constexpr_indexed&& that) : _index{that._index} {
      std::move(that).template visit<void>([this] <typename T> (T&& x) { std::construct_at(&data.template get<T>(), std::move(x)); });
    }

    constexpr ~variant_constexpr_indexed() {
      visit<void>([](auto& x) { std::destroy_at(&x); });
    }
};

#endif
