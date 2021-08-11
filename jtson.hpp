#ifndef jtson_hpp
#define jtson_hpp

#include <algorithm>
#include <array>
#include <concepts>
#include <functional>
#include <iostream>
#include <iterator>
#include <limits>
#include <memory>
#include <ostream>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <unordered_map>
#include <variant>
#include <vector>

#include "trie.hpp"
#include "unique_ptr_constexpr.hpp"
#include "unique_ptr_soo.hpp"
#include "variant_constexpr.hpp"
#include "vector_constexpr.hpp"

using namespace std::literals;

namespace {
  template <auto> struct lift_to_type {};

  template <typename T> struct proxy {
    using type = T;
  };

  template<std::size_t N>
  struct string_literal {
    char value[N];

    constexpr string_literal(char const (&str)[N]) {
      std::copy_n(str, N, value);
    }

    constexpr auto operator[](std::size_t i) const { return value[i]; }

    constexpr operator std::string_view() const {
      return std::string_view{value, N - 1};
    }
  };

  template <std::size_t, typename T>
  struct indexed_base_class : T {};

}

namespace json {
  struct array;
  class object;

  struct null {
    friend auto operator<<(std::ostream& os, null) -> std::ostream& {
      return os << "null";
    }
  };

  // TODO delegate to std::string once constexpr std::string is available
  class string {
    private:
      static constexpr char empty[] = "";
      char const * data;
      std::size_t size;

    public:
      constexpr string() : data{empty}, size{0} {}

      constexpr string(std::string_view sv)
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

      constexpr string(string const & that) : string{that.operator std::string_view()} {}

      constexpr ~string() { if (data != empty) { delete[] data; } }

      constexpr auto c_str() const -> char const * { return data; }

      constexpr operator std::string_view() const { return {data, size}; }

      friend auto operator<<(std::ostream& os, string const & str) -> std::ostream& {
        return os << '"' << str.operator std::string_view() << '"';
      }
  };

  namespace literals {
    constexpr auto operator""_jstr(char const * str, std::size_t len) {
      return string{{str, len}};
    }
  }
}

namespace std {
  template<>
  struct hash<json::string> : hash<std::string> {};
}

namespace json {
  namespace impl {
    struct _unique_ptr_constexpr {
      template <typename T>
      using type = unique_ptr_constexpr<T>;

      static constexpr auto is_owning = true;

      static constexpr auto is_trivially_copy_constructible = false;

      static constexpr auto construct(auto&& x) -> unique_ptr_constexpr<std::decay_t<decltype(x)>> {
        return make_unique_constexpr<std::decay_t<decltype(x)>>(std::forward<decltype(x)>(x));
      }
    };

    struct raw_ptr {
      template <typename T>
      using type = T*;

      static constexpr auto is_owning = false;

      static constexpr auto is_trivially_copy_constructible = false;

      static constexpr auto construct(auto& x) { return &x; }
    };

    struct const_raw_ptr {
      template <typename T>
      using type = T const *;

      static constexpr auto is_owning = false;

      static constexpr auto is_trivially_copy_constructible = false;

      static constexpr auto construct(auto const & x) { return &x; }
    };

    template <typename ptr>
    class basic_value {
      private:
        template <typename T>
        using ptr_t = typename ptr::template type<T>;

        using variant_type =
          variant_constexpr
            < ptr_t<null>
            , ptr_t<long>
            , ptr_t<double>
            , ptr_t<string>
            , ptr_t<bool>
            , ptr_t<array>
            , ptr_t<object>
            >;

          variant_type datum;

        template <typename>
        friend class basic_value;

      public:
        constexpr basic_value(basic_value const &) requires ptr::is_trivially_copy_constructible = default;

        constexpr basic_value(basic_value const & that)
          : datum{that.datum.template visit<variant_type>([](auto& x) -> variant_type { return variant_type{ptr::construct(*x)}; })}
          {}

        template <typename ptr2>
        constexpr basic_value(basic_value<ptr2> & that)
          : datum{that.datum.template visit<variant_type>([](auto& x) -> variant_type { return variant_type{ptr::construct(*x)}; })}
          {}

        template <typename ptr2>
        constexpr basic_value(basic_value<ptr2> const & that)
          : datum{that.datum.template visit<variant_type>([](auto& x) -> variant_type { return variant_type{ptr::construct(*x)}; })}
          {}

        constexpr basic_value& operator=(basic_value const & that) {
          auto tmp = that;
          std::swap(*this, tmp);
          return *this;
        }

        constexpr basic_value(basic_value&&) = default;
        constexpr basic_value& operator=(basic_value&&) = default;

        constexpr basic_value(auto&& x) requires (variant_type::template is_of_variant<ptr_t<std::decay_t<decltype(x)>>>) : datum{ptr::construct(std::forward<decltype(x)>(x))} {};

        constexpr basic_value(int x) : basic_value{static_cast<long>(x)} {
          static_assert(ptr::is_owning, "Cannot construct view from wrong type");
        };
        constexpr basic_value(char const * x) : basic_value{json::string{x}} {
          static_assert(ptr::is_owning, "Cannot construct view from wrong type");
        };

        constexpr auto is_null() const {
          return datum.template holds<ptr_t<null>>();
        }

        constexpr auto is_number() const {
          return
            (  datum.template holds<ptr_t<long>>()
            || datum.template holds<ptr_t<double>>()
            );
        }

        constexpr auto is_string() const {
          return datum.template holds<ptr_t<string>>();
        }

        constexpr auto as_int() const -> long const * {
          if (auto x = datum.template get_if<ptr_t<long>>()) {
            return &**x;
          } else {
            return nullptr;
          }
        }

        constexpr auto as_string() const -> string const * {
          if (auto x = datum.template get_if<ptr_t<string>>()) {
            return &**x;
          } else {
            return nullptr;
          }
        }

        constexpr auto as_array() const -> array const * {
          if (auto x = datum.template get_if<ptr_t<array>>()) {
            return &**x;
          } else {
            return nullptr;
          }
        }

        constexpr auto as_object() const -> object const * {
          if (auto x = datum.template get_if<ptr_t<object>>()) {
            return &**x;
          } else {
            return nullptr;
          }
        }

        constexpr auto debug_type() const {
          return
            datum.template match<std::string_view>
              ( [](ptr_t<null>   const &) { return "null"; }
              , [](ptr_t<long>   const &) { return "int"; }
              , [](ptr_t<double> const &) { return "float"; }
              , [](ptr_t<string> const &) { return "string"; }
              , [](ptr_t<bool>   const &) { return "bool"; }
              , [](ptr_t<array>  const &) { return "array"; }
              , [](ptr_t<object> const &) { return "object"; }
              );
        }

        friend constexpr auto operator<<(std::ostream& os, basic_value const & val) -> std::ostream& {
          return val.datum.template visit<std::ostream&>([&](auto const & x) -> std::ostream& { return os << *x; });
        } 
    };
  }

  using value = impl::basic_value<impl::_unique_ptr_constexpr>;
  using value_mutable_view = impl::basic_value<impl::raw_ptr>;
  using value_const_view = impl::basic_value<impl::const_raw_ptr>;

  struct array : vector_constexpr<value> {
    constexpr array(array&) = default;
    constexpr array& operator=(array&) = default;
    constexpr array(array const &) = default;
    constexpr array& operator=(array const &) = default;
    constexpr array(array&&) = default;
    constexpr array& operator=(array&&) = default;

    constexpr array(std::convertible_to<value> auto&& ...vals) : vector_constexpr<value>{std::forward<decltype(vals)>(vals)...} {}

    friend auto operator<<(std::ostream& os, array const & arr) -> std::ostream& {
      os << "[";
      auto it = arr.begin();
      if (it != arr.end()) {
        os << *it;
        ++it;
      }
      for (; it != arr.end(); ++it) {
        os << "," << *it;
      }
      os << "]";
      return os;
    }
  };

  class object {
    private:
      trie<value> data;

    public:
      constexpr object(object&) = default;
      constexpr object(object const &) = default;
      constexpr object& operator=(object const &) = default;
      constexpr object(object&&) = default;
      constexpr object& operator=(object&&) = default;

      constexpr object(auto ...args) {
        ([&] {
          auto& [key, val] = args;
          data.emplace(std::move(key), std::move(val));
        }(), ...);
      }

      constexpr auto get_if(std::string_view key) const -> value const * {
        return data.get_if(key);
      }

      friend auto operator<<(std::ostream& os, object const & obj) -> std::ostream& {
        os << "{";
        auto it = obj.data.begin();
        if (it != obj.data.end()) {
          os << it->first << ":" << it->second;
          ++it;
        }
        for (; it != obj.data.end(); ++it) {
          os << "," << it->first << ":" << it->second;
        }
        os << "}";
        return os;
      }
  };

  namespace typed {
    struct parsed_wrong_type : std::runtime_error { using std::runtime_error::runtime_error; };

    template <typename>
    struct parse_t;

    template <typename T> constexpr auto parse(json::value_const_view val) { return parse_t<T>{}(val); }

    
    template <string_literal Ident, typename T>
    struct decl {
      static auto _lookup(lift_to_type<Ident>) -> T { return std::declval<T>(); }
    };

    namespace impl {
      template <typename> constexpr auto _is_decl = false;
      template <string_literal Ident, typename T> constexpr auto _is_decl<decl<Ident, T>> = true;
      template <typename Decl> concept is_decl = _is_decl<Decl>;
    }

    namespace impl {
      template <typename> constexpr auto _is_context = false;
      template <typename Context> concept is_context = _is_context<Context>;
    }

    template <typename>
    struct in_context_t;

    template <impl::is_context context, typename T> using in_context = typename in_context_t<T>::type<context>;

    template <impl::is_decl ...Decls> struct context : private Decls... {
      private:
        using Decls::_lookup...;

      public:
        template <string_literal Ident> using lookup = in_context<context, decltype(_lookup(lift_to_type<Ident>{}))>;
    };

    namespace impl {
      template <is_decl ...Decls> constexpr auto _is_context<context<Decls...>> = true;
    }

    template <>
    struct parse_t<json::value> {
      constexpr auto operator()(json::value_const_view val) -> json::value {
        return val;
      }
    };

    constexpr auto untyped(json::value const & val) -> json::value const & { return val; }
    constexpr auto untyped(json::value &&      val) -> json::value &&      { return std::move(val); }

    template <>
    struct in_context_t<json::value> {
      template <impl::is_context> using type = json::value;
    };

    template <>
    struct parse_t<long> {
      constexpr auto operator()(json::value_const_view val) -> long {
        if (auto x = val.as_int()) {
          return *x;
        } else {
          throw parsed_wrong_type{"Expected an integer, got "s + std::string{val.debug_type()}};
        }
      }
    };

    constexpr auto untyped(long x) { return x; }

    template <>
    struct in_context_t<long> {
      template <impl::is_context> using type = long;
    };

    template <>
    struct parse_t<json::string> {
      constexpr auto operator()(json::value_const_view val) -> json::string {
        if (auto str = val.as_string()) {
          return *str;
        } else {
          throw parsed_wrong_type{"Expected a string, got "s + std::string{val.debug_type()}};
        }
      }
    };

    constexpr auto untyped(json::string const & str) -> json::string const & { return str; }
    constexpr auto untyped(json::string&& str) -> json::string&& { return std::move(str); }

    template <>
    struct in_context_t<json::string> {
      template <impl::is_context> using type = json::string;
    };

    template <typename T>
    struct parse_t<vector_constexpr<T>> {
      constexpr auto operator()(json::value_const_view val) -> vector_constexpr<T> {
        if (auto xs = val.as_array()) {
          auto ys = vector_constexpr<T>(xs->size());
          std::transform(xs->begin(), xs->end(), std::back_insert_iterator{ys}, parse_t<T>{});
          return ys;
        } else {
          throw parsed_wrong_type{"Expected an array, got "s + std::string{val.debug_type()}};
        }
      }
    };

    constexpr auto untyped(vector_constexpr<auto> x) -> json::array { throw 0;/* TODO */ }

    template <typename T>
    struct in_context_t<vector_constexpr<T>> {
      template <impl::is_context Context> using type = vector_constexpr<in_context<Context, T>>;
    };

    template <string_literal Name, typename T>
    struct field {
      private:
        T datum;

      public:
        static constexpr auto name = Name;
  
        constexpr field(auto&& ...args) : datum{std::forward<decltype(args)>(args)...} {}
  
        constexpr field(json::object const & obj)
          : datum
            { [&] {
                if (auto x = obj.get_if(Name)) {
                  try {
                    return parse<T>(*x);
                  } catch (parsed_wrong_type const & e) {
                    throw parsed_wrong_type{"When parsing field: "s + std::string{Name} + "\n"s + e.what()};
                  }
                } else {
                  throw parsed_wrong_type{"Expected field: "s + std::string{Name}};
                }
              }()
            }
          {}
  
        constexpr auto get_field(lift_to_type<Name>) &       -> auto &       { return datum; }
        constexpr auto get_field(lift_to_type<Name>) const & -> auto const & { return datum; }
        constexpr auto get_field(lift_to_type<Name>) &&      -> auto &&      { return datum; }
    };

    template <string_literal Name, typename T>
    struct in_context_t<field<Name, T>> {
      template <impl::is_context Context> using type = field<Name, in_context<Context, T>>;
    };

    template <string_literal Name, impl::is_context Context, string_literal Ident>
    struct lazy_field {
      private:
        using type = Context::template lookup<Ident>;

        unique_ptr_constexpr<type> datum;

      public:
        static constexpr auto name = Name;

        constexpr lazy_field(auto&& ...args) : datum{new type{std::forward<decltype(args)>(args)...}} {}
  
        constexpr lazy_field(json::object const & obj)
          : datum
            { [&] {
                if (auto x = obj.get_if(Name)) {
                  try {
                    return make_unique_constexpr<type>(parse<type>(*x));
                  } catch (parsed_wrong_type const & e) {
                    throw parsed_wrong_type{"When parsing field: "s + std::string{Name} + "\n"s + e.what()};
                  }
                } else {
                  throw parsed_wrong_type{"Expected field: "s + std::string{Name}};
                }
              }()
            }
          {}

        constexpr auto get_field(lift_to_type<Name>) &       -> auto &       { return *datum; }
        constexpr auto get_field(lift_to_type<Name>) const & -> auto const & { return *datum; }
        constexpr auto get_field(lift_to_type<Name>) &&      -> auto &&      { return *datum; }
    };

    template <string_literal Name, string_literal Ident>
    struct rec_field;

    template <string_literal Name, string_literal Ident>
    struct in_context_t<rec_field<Name, Ident>> {
      template <impl::is_context Context> using type = lazy_field<Name, Context, Ident>;
    };

    namespace impl {
      template <typename>
      constexpr auto _is_field = false;

      template <string_literal Name, typename T>
      constexpr auto _is_field<field<Name, T>> = true;

      template <string_literal Name, impl::is_context Context, string_literal Ident>
      constexpr auto _is_field<lazy_field<Name, Context, Ident>> = true;

      template <typename Field> concept is_field = _is_field<Field>;
    }

    template <impl::is_field ...Fields>
    class object : private Fields... {
      private:
        using Fields::get_field...;

      public:
        template <string_literal Name>
        constexpr auto get() & -> auto & {
          return get_field(lift_to_type<Name>{});
        }

        template <string_literal Name>
        constexpr auto get() const & -> auto const & {
          return get_field(lift_to_type<Name>{});
        }

        template <string_literal Name>
        constexpr auto get() && -> auto && {
          return get_field(lift_to_type<Name>{});
        }

        constexpr object(Fields ...fields) : Fields{std::move(fields)}... {}

        template <typename ...Fields2>
        constexpr object(object<Fields2...> const & that) : Fields{that.template get<Fields::tag>()}... {}

        template <typename ...Fields2>
        constexpr object(object<Fields2...> && that) : Fields{std::move(that).template get<Fields::tag>()}... {}

        constexpr object(json::object const & obj) : Fields{obj}... {}
  
        friend constexpr auto untyped(object const & obj) {
          return json::object{std::pair{Fields::tag, obj.get<Fields::tag>().untyped()}...};
        }
        
        friend constexpr auto untyped(object && obj) {
          return json::object{std::pair{Fields::tag, std::move(obj).template get<Fields::tag>().untyped()}...};
        }
    };

    template <impl::is_field ...Fields>
    struct parse_t<object<Fields...>> {
      constexpr auto operator()(json::value_const_view val) -> object<Fields...> {
        if (auto obj = val.as_object()) {
          return object<Fields...>{*obj};
        } else {
          throw parsed_wrong_type{"Expected an object"};
        }
      }
    };

    template <impl::is_field ...Fields>
    struct in_context_t<object<Fields...>> {
      template <impl::is_context Context> using type = object<in_context<Context, Fields>...>;
    };

    template <typename ...Fields1, typename ...Fields2>
    constexpr auto operator|(object<Fields1...> lhs, object<Fields2...> rhs) -> object<Fields1..., Fields2...> {
      return {std::move(lhs).template get<Fields1::tag>()..., std::move(rhs).template get<Fields2::tag>()...};
    }

    template <typename ...Fields>
    class rec_object;

    template <typename ...Fields>
    struct in_context_t<rec_object<Fields...>> {
      template <impl::is_context Context> using type = object<in_context<Context, Fields>...>;
    };

    template <string_literal Tag, impl::is_field ...Fields>
    struct union_case : object<Fields...> {
      static constexpr auto tag = Tag;
      using type = object<Fields...>;

      using object<Fields...>::object;

      static constexpr auto get_type(lift_to_type<Tag>) -> object<Fields...> { return std::declval<object<Fields...>>(); }
      static constexpr auto get_case(lift_to_type<Tag>) -> union_case { return std::declval<union_case>(); }
    };

    template <string_literal Tag, impl::is_field ...Fields>
    struct in_context_t<union_case<Tag, Fields...>> {
      template <impl::is_context Context> using type = union_case<Tag, in_context<Context, Fields>...>;
    };

    template <string_literal Tag, typename ...Fields>
    struct rec_union_case;

    template <string_literal Tag, typename ...Fields>
    struct in_context_t<rec_union_case<Tag, Fields...>> {
      template <impl::is_context Context> using type = union_case<Tag, in_context<Context, Fields>...>;
    };

    template <string_literal Tag, typename CRTP>
    struct match_case_base {
      constexpr auto get_func(lift_to_type<Tag>) const -> auto const & {
        return static_cast<CRTP const *>(this)->f;
      }
    };

    template <typename F, string_literal ...Tags>
    struct match_case_t : match_case_base<Tags, match_case_t<F, Tags...>>... {
      using match_case_base<Tags, match_case_t>::get_func...;

      F const & f;

      constexpr match_case_t(F const & f) : f{f} {}

      constexpr auto operator()(auto&& ...args) -> decltype(auto) {
        return f(std::forward<decltype(args)>(args)...);
      }
    };

    template <string_literal ...Tags, typename F>
    constexpr auto match_case(F const & f) {
      return match_case_t<F, Tags...>{f};
    }

    namespace impl {
      template <typename>
      constexpr auto _is_case = false;

      template <string_literal Tag, typename ...Fields>
      constexpr auto _is_case<union_case<Tag, Fields...>> = true;

      template <typename Case>
      concept is_case = _is_case<Case>;

      template <std::size_t I, typename T>
      struct index_in_indexed_pack_base {
        static constexpr auto index(proxy<T>) { return I; }
      };

      template <typename Is, typename ...Ts>
      struct index_in_indexed_pack;

      template <std::size_t ...Is, typename ...Ts>
      struct index_in_indexed_pack<std::index_sequence<Is...>, Ts...> : index_in_indexed_pack_base<Is, Ts>... {
        using index_in_indexed_pack_base<Is, Ts>::index...;
      };

      template <typename T, typename ...Ts>
      constexpr int index_in_pack = index_in_indexed_pack<std::index_sequence_for<Ts...>, Ts...>::index(proxy<T>{});

      template <typename ...Cases>
      struct type_from_cases : Cases... {
        using Cases::get_type...;
        using Cases::get_case...;

        template <string_literal Tag>
        using type = decltype(get_type(lift_to_type<Tag>{}));

        template <string_literal Tag>
        using case_type = decltype(get_case(lift_to_type<Tag>{}));
      };

      template <typename>
      constexpr auto _is_match_case = false;

      template <typename F, string_literal ...Tags>
      constexpr auto _is_match_case<match_case_t<F, Tags...>> = true;

      template <typename MatchCase>
      concept is_match_case = _is_match_case<MatchCase>;

      template <typename ...MatchCases>
      struct get_match_case : MatchCases... {
        using MatchCases::get_func...;
      };

      template <typename From, typename To, typename>
      struct cast_integer_sequence;

      template <typename From, typename To, From ...xs>
      struct cast_integer_sequence<From, To, std::integer_sequence<From, xs...>> {
        using type = std::integer_sequence<To, static_cast<To>(xs)...>;
      };

      using all_chars = cast_integer_sequence<int, char, std::make_integer_sequence<int, static_cast<int>(std::numeric_limits<unsigned char>::max()) + 1>>::type;

      template <string_literal Tag>
      struct case_constructor_stub {
        static constexpr auto tag = Tag;

        static constexpr auto done = true;
        static constexpr auto leaf = false;

        using stub = case_constructor_stub;

        template <char> using advance = case_constructor_stub;

        static constexpr void get();
      };
    }

    template <string_literal Discriminator, impl::is_case ...Cases>
    struct discriminated_union : private variant_constexpr<Cases...> {
      public:
        using variant_constexpr<Cases...>::variant_constexpr;
        using variant_constexpr<Cases...>::visit;

        template <typename Ret>
        constexpr auto match(impl::is_match_case auto ...cases) & -> Ret {
          return static_cast<variant_constexpr<Cases...>&>(*this).template match<Ret>(impl::get_match_case{cases...}.get_func(lift_to_type<Cases::tag>{})...);
        }

        template <typename Ret>
        constexpr auto match(impl::is_match_case auto ...cases) const & -> Ret {
          return static_cast<variant_constexpr<Cases...> const &>(*this).template match<Ret>(impl::get_match_case{cases...}.get_func(lift_to_type<Cases::tag>{})...);
        }

        template <typename Ret>
        constexpr auto match(impl::is_match_case auto ...cases) && -> Ret {
          return static_cast<variant_constexpr<Cases...>&&>(*this).template match<Ret>(impl::get_match_case{cases...}.get_func(lift_to_type<Cases::tag>{})...);
        }

        template <string_literal Tag>
        constexpr auto get_if() const -> auto const * {
          return get_if<impl::type_from_cases<Cases...>::template type<Tag>>();
        }

        friend constexpr auto untyped(discriminated_union const & x) {
          constexpr auto tags = std::array{Cases::tag...};
          return x.template visit<json::object>
            ( [tag = tags[x.index()]] <typename ...Fields> (object<Fields...> const & obj) {
                return json::object
                  { std::pair{Discriminator, json::string{tag}}
                  , std::pair{Fields::name, untyped(obj.template get<Fields::name>())}...
                  };
              }
            );
        }

        friend constexpr auto untyped(discriminated_union && x) {
          constexpr auto tags = std::array{Cases::tag...};
          return std::move(x).template visit<json::object>
            ( [tag = tags[x.index]] <typename ...Fields> (object<Fields...>&& obj) {
                return json::object
                  { std::pair{Discriminator, tag}
                  , std::pair{Fields::name, untyped(std::move(obj).template get<Fields::name>())}...
                  };
              }
            );
        }
    };

    template <string_literal Discriminator, impl::is_case ...Cases>
    struct parse_t<discriminated_union<Discriminator, Cases...>> {
      private:
        template <typename Case, std::size_t offset = 0>
        struct case_constructor {
          static constexpr auto tag = Case::tag;
  
          static constexpr auto done = false;
          static constexpr auto leaf = tag[offset] == '\0';
  
          using stub = impl::case_constructor_stub<Case::tag>;

          template <char c>
          using advance = std::conditional_t<tag[offset] == c, case_constructor<Case, offset + 1>, stub>;
  
          static constexpr auto get(char const *, json::object const & x) -> discriminated_union<Discriminator, Cases...> requires leaf { return {Case{x}}; }
        };
  
        template <typename, typename ...CaseConstructors>
        struct tag_trie_indexed;
  
        template <typename ...CaseConstructors>
        using tag_trie = tag_trie_indexed<impl::all_chars, CaseConstructors...>;
  
        template <char ...cs, typename ...CaseConstructors>
        struct tag_trie_indexed<std::integer_sequence<char, '\0', cs...>, CaseConstructors...> : private CaseConstructors... {
          private:
            using CaseConstructors::get...;
  
            static constexpr auto get(char const *, json::object const &) -> discriminated_union<Discriminator, Cases...> requires (!CaseConstructors::leaf && ... && true) {
              throw parsed_wrong_type{"Invalid tag value"};
            }
  
            template <char c>
            static constexpr auto advance_then_lookup = tag_trie<typename CaseConstructors::template advance<c>...>::lookup;
  
          public:
            static constexpr auto lookup(char const * tag, json::object const & x) -> discriminated_union<Discriminator, Cases...> {
              if constexpr ((CaseConstructors::done && ... && true)) {
                throw parsed_wrong_type{"Invalid tag value"};
              } else {
                using func_ptr = auto (*)(char const *, json::object const &) -> discriminated_union<Discriminator, Cases...>;
                constexpr auto fs = std::array{static_cast<func_ptr>(get), advance_then_lookup<cs>...};
                return fs[static_cast<unsigned char>(*tag)](tag + 1, x);
              }
            }
        };

      public:
        constexpr auto operator()(json::value_const_view val) -> discriminated_union<Discriminator, Cases...> {
          if (auto obj = val.as_object()) {
            if (auto tag_val = obj->get_if(Discriminator)) {
              if (auto tag = tag_val->as_string()) {
                return tag_trie<case_constructor<Cases>...>::lookup(tag->c_str(), *obj);
              } else {
                throw parsed_wrong_type{"Discriminator field is not a string"};
              }
            } else {
              throw parsed_wrong_type{"Could not find discriminator field"};
            }
          } else {
            throw parsed_wrong_type{"Expected an object"};
          }
        }
    };

    template <string_literal Discriminator, impl::is_case ...Cases>
    struct in_context_t<discriminated_union<Discriminator, Cases...>> {
      template <impl::is_context Context> using type = discriminated_union<Discriminator, in_context<Context, Cases>...>;
    };

    template <string_literal Discriminator, typename ...Cases>
    struct rec_discriminated_union;

    template <string_literal Discriminator, typename ...Cases>
    struct in_context_t<rec_discriminated_union<Discriminator, Cases...>> {
      template <impl::is_context Context> using type = discriminated_union<Discriminator, in_context<Context, Cases>...>;
    };
  }

  namespace validator {
    template <typename T>
    concept validator = requires(T vldtr, value_const_view val) {
      { vldtr(val) } -> std::same_as<bool>;
    };

    class any_validator {
      private:
        unique_ptr_soo<sizeof(void*), void> datum;
        auto (*call)(void const * _this, value_const_view val) -> bool;

      public:
        any_validator(any_validator&) = default;
        any_validator(any_validator const &) = default;
        any_validator(any_validator&&) = default;

        any_validator(validator auto&& x)
          : datum{std::forward<decltype(x)>(x)}
          , call
            { [](void const * _this, value_const_view val) -> bool {
                return (*static_cast<std::decay_t<decltype(x)> const *>(_this))(val);
              }
            }
          {}

        auto operator()(value_const_view val) const -> bool {
          return call(datum.get(), val);
        }
    };

    struct any {
      auto operator()(value_const_view) const -> bool {
        return true;
      }
    };

    template <typename Child>
    struct _not : private Child {
      using Child::Child;

      auto operator()(value_const_view val) const -> bool {
        return !static_cast<Child const &>(*this)(val);
      }
    };

    namespace impl {
      template <typename, validator ...Clauses>
      struct and_indexed;

      template <std::size_t ...Is, validator ...Clauses>
      struct and_indexed<std::index_sequence<Is...>, Clauses...>
        : private indexed_base_class<Is, Clauses>...
      {
        and_indexed() = default;
        and_indexed(Clauses ...clauses) : indexed_base_class<Is, Clauses>{std::move(clauses)}... {}

        auto operator()(value_const_view val) const -> bool {
          return (static_cast<indexed_base_class<Is, Clauses> const &>(*this)(val) && ...);
        }
      };

      template <typename, validator ...Clauses>
      struct or_indexed;

      template <std::size_t ...Is, validator ...Clauses>
      struct or_indexed<std::index_sequence<Is...>, Clauses...>
        : private indexed_base_class<Is, Clauses>...
      {
        or_indexed() = default;
        or_indexed(Clauses ...clauses) : indexed_base_class<Is, Clauses>{std::move(clauses)}... {}

        auto operator()(value_const_view val) const -> bool {
          return (static_cast<indexed_base_class<Is, Clauses> const &>(*this)(val) || ...);
        }
      };
    }

    template <validator ...Clauses>
    using _and = impl::and_indexed<std::index_sequence_for<Clauses...>, Clauses...>;

    template <validator ...Clauses>
    using _or = impl::or_indexed<std::index_sequence_for<Clauses...>, Clauses...>;

    namespace impl {
      template <auto (value_const_view::*member)() const -> bool>
      struct primitive {
        auto operator()(value_const_view val) const -> bool {
          return (val.*member)();
        }
      };
    }

    struct null   : impl::primitive<&value_const_view::is_null> {};
    struct number : impl::primitive<&value_const_view::is_number> {};
    struct string : impl::primitive<&value_const_view::is_string> {};

    template <validator Element>
    struct array : private Element {
      using Element::Element;

      auto operator()(value_const_view val) const -> bool {
        if (auto arr = val.as_array()) {
          for (auto const & x : *arr) {
            if (!static_cast<Element const &>(*this)(x)) {
              return false;
            }
          }
          return true;
        } else {
          return false;
        }
      }
    };

    template <string_literal Name, validator Contents>
    struct field {
      static constexpr auto name = Name;
      using contents = Contents;
    };

    namespace impl {
      template <typename> constexpr auto _is_field = false;
      template <string_literal Name, typename Contents> constexpr auto _is_field<field<Name, Contents>> = true;

      template <typename Field> concept is_field = _is_field<Field>;
    }

    template <impl::is_field Field>
    struct contains_field : private Field::contents {
      using Field::contents::contents;

      auto operator()(json::object const & obj) const -> bool {
        if (auto x = obj.get_if(Field::name)) {
          return static_cast<Field::contents const &>(*this)(*x);
        } else {
          return false;
        }
      }

      auto operator()(value_const_view val) const -> bool {
        if (auto obj = val.as_object()) {
          return (*this)(*obj);
        } else {
          return false;
        }
      }
    };

    template <impl::is_field ...Fields>
    struct object : _and<contains_field<Fields>...> {};

    template <string_literal Tag, validator Validator>
    struct union_case {
      static constexpr auto tag = Tag;
      using validator = Validator;
    };

    namespace impl {
      template <typename> constexpr auto _is_case = false;
      template <string_literal Tag, validator Validator> constexpr auto _is_case<union_case<Tag, Validator>> = true;

      template <typename Case> concept is_case = _is_case<Case>;
    }

    template <string_literal Discriminator, impl::is_case Case>
    struct inhabits_case : private Case::validator {
      using Case::validator::validator;

      auto operator()(json::object const & obj) const -> bool {
        if (auto tagVal = obj.get_if(json::string{Discriminator})) {
          if (auto tag = tagVal->as_string()) {
            if (*tag == Case::tag) {
              return static_cast<Case::validator const &>(*this)(obj);
            }
          }
        }
        return false;
      }

      auto operator()(value_const_view val) const -> bool {
        if (auto obj = val.as_object()) {
          return (*this)(*obj);
        } else {
          return false;
        }
      }
    };

    template <string_literal Discriminator, impl::is_case ...Cases>
    struct discriminated_union : _or<inhabits_case<Discriminator, Cases>...> {};
  }
}

#endif
