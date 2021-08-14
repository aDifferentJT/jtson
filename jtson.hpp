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

#include "polyfill.hpp"
#include "string_constexpr.hpp"
#include "trie.hpp"
#include "unique_ptr_constexpr.hpp"
#include "unique_ptr_soo.hpp"
#include "variant_constexpr.hpp"
#include "vector_constexpr.hpp"

using namespace std::literals;

namespace {
  template <auto x, typename ...> constexpr auto always = x;

  template <auto...> struct lift_to_type {};

  template <typename> struct proxy {};

  template<std::size_t N>
  struct string_literal {
    static constexpr auto size = N - 1;

    char value[N];

    constexpr string_literal(char const (&str)[N]) { std::copy_n(str, N, value); }

    constexpr auto operator[](std::size_t i) const { return value[i]; }

    template <std::size_t pos = 0, std::size_t count = std::numeric_limits<std::size_t>::max()>
    constexpr auto substr() const {
      constexpr auto _count = std::min(count, size - pos);
      char res[_count + 1];
      std::copy_n(value + pos, _count, res);
      res[_count] = '\0';
      return string_literal<_count + 1>{res};
    }

    constexpr auto empty() const { return size == 0; }

    constexpr auto begin()       -> char       * { return value; }
    constexpr auto begin() const -> char const * { return value; }
    constexpr auto end()       -> char       * { return value + size; }
    constexpr auto end() const -> char const * { return value + size; }

    constexpr auto view() const { return std::string_view{value, size}; }
    constexpr operator std::string_view() const { return view(); }

    constexpr auto find(std::string_view str) const { return view().find(str); }
    constexpr auto find(char ch) const { return view().find(ch); }
    constexpr auto starts_with(std::string_view prefix) const { return view().starts_with(prefix); }
    constexpr auto starts_with(char ch) const { return view().starts_with(ch); }

    constexpr auto find_first_non_whitespace() const { return view().find_first_not_of(" \n"); }
  };

  template <string_literal str>
  constexpr auto trim_leading() {
    constexpr auto i = str.find_first_non_whitespace();
    if constexpr (i == std::string_view::npos) {
      return string_literal<1>{""};
    } else {
      return str.template substr<i>();
    }
  }

  template <std::size_t, typename T>
  struct indexed_base_class : T {};
}

namespace json {
  void pretty
    ( std::ostream& os
    , auto const & x
    , std::string const & inline_indent = ""
    , [[maybe_unused]] std::string const & first_indent = ""
    , [[maybe_unused]] std::string const & rest_indent = "\n"
    )
  {
    auto flags = os.flags();
    os << inline_indent << std::boolalpha << x;
    os.flags(flags);
  }

  struct array;
  struct object;

  auto operator<<(std::ostream& os, array const & obj) -> std::ostream&;
  auto operator<<(std::ostream& os, object const & obj) -> std::ostream&;

  struct null {
    friend auto operator<<(std::ostream& os, null) -> std::ostream& {
      return os << "null";
    }
  };

  // TODO delegate to std::string once constexpr std::string is available
  struct string : string_constexpr {
    using string_constexpr::string_constexpr;

    friend auto operator<<(std::ostream& os, string const & str) -> std::ostream& {
      return os << '"' << static_cast<string_constexpr const &>(str) << '"';
    }
  };

  namespace literals {
    constexpr auto operator""_jstr(char const * str, std::size_t len) {
      return string{{str, len}};
    }
  }

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
        }
        constexpr basic_value(char const * x) : basic_value{json::string{x}} {
          static_assert(ptr::is_owning, "Cannot construct view from wrong type");
        }
        constexpr basic_value(bool x) : datum{ptr::construct(x)} {};

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

        constexpr auto as_float() const -> double const * {
          if (auto x = datum.template get_if<ptr_t<double>>()) {
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

        constexpr auto as_bool() const -> bool const * {
          if (auto x = datum.template get_if<ptr_t<bool>>()) {
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
          return val.datum.template visit<std::ostream&>
            ( [&](auto const & x) -> std::ostream& {
                auto flags = os.flags();
                os << std::boolalpha << *x;
                os.flags(flags);
                return os;
              }
            );
        } 

        friend void pretty
          ( std::ostream& os
          , basic_value const & val
          , std::string const & inline_indent = ""
          , std::string const & first_indent = ""
          , std::string const & rest_indent = "\n"
          )
        {
          return val.datum.template visit<void>([&](auto const & x) { pretty(os, *x, inline_indent, first_indent, rest_indent); });
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

    constexpr array(polyfill::convertible_to<value> auto&& ...vals) : vector_constexpr<value>{std::forward<decltype(vals)>(vals)...} {}

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

    friend void pretty
      ( std::ostream& os
      , array const & arr
      , [[maybe_unused]] std::string const & inline_indent = ""
      , std::string const & first_indent = ""
      , std::string const & rest_indent = "\n"
      )
    {
      auto it = arr.begin();
      if (it != arr.end()) {
        pretty(os, *it, first_indent + "[ ", first_indent + "[ ", rest_indent + "  ");
        ++it;
        for (; it != arr.end(); ++it) {
          pretty(os, *it, first_indent + ", ", first_indent + ", ", rest_indent + "  ");
        }
        os << rest_indent << "]";
      } else {
        os << first_indent << "[]";
      }
    }
  };

  namespace impl {
    constexpr auto is_initial_ident_char(char c) {
      return c == '_' || ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z');
    }

    constexpr auto is_ident_char(char c) {
      return is_initial_ident_char(c) || ('9' <= c && c <= '9');
    }
  }

  struct object : trie<value> {
    using trie<value>::trie;

    friend auto operator<<(std::ostream& os, object const & obj) -> std::ostream& {
      os << "{";
      auto it = obj.begin();
      if (it != obj.end()) {
        os << '"' << it->first << '"' << ':' << it->second;
        ++it;
      }
      for (; it != obj.end(); ++it) {
        os << ",\"" << it->first << '"' << ':' << it->second;
      }
      os << "}";
      return os;
    }

    friend void pretty
      ( std::ostream& os
      , object const & obj
      , [[maybe_unused]] std::string const & inline_indent = ""
      , std::string const & first_indent = ""
      , std::string const & rest_indent = "\n"
      )
    {
      os << first_indent << "{";
      auto it = obj.begin();
      if (it != obj.end()) {
        os << " \"" << it->first << "\" :";
        pretty(os, it->second, " ", rest_indent + "  ", rest_indent + "  ");
        ++it;
      }
      for (; it != obj.end(); ++it) {
        os << rest_indent << ", \"" << it->first << "\" :";
        pretty(os, it->second, " ", rest_indent + "  ", rest_indent + "  ");
      }
      os << rest_indent << "}";
    }
  };

  struct parse_error : std::runtime_error {
    std::string_view where;

    parse_error(std::string const & what, std::string_view where)
      : std::runtime_error{what + ": \"" + std::string(where.substr(0, 10)) + "\""}
      , where{where}
      {}
  };

  namespace impl {
    constexpr void eat_whitespace(std::string_view& str) {
      while (true) {
        if (str.empty()) { return; }
        switch (str[0]) {
          case ' ':
          case '\n':
            str = str.substr(1);
            break;
          default:
            return;
        }
      }
    }

    constexpr auto parse(std::string_view& str) -> json::value {
      if (str.empty()) {
        throw parse_error{"Empty string", str};
      }
      eat_whitespace(str);
      switch (str[0]) {
        case 'n':
          if (str[1] == 'u' && str[2] == 'l' && str[3] == 'l') {
            str = str.substr(4);
            return null{};
          } else {
            throw parse_error{"Saw 'n', expected \"null\"", str};
          }
        case '+':
        case '-':
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
          throw parse_error{"Unimplemented parsing numbers", str}; // TODO
        case '"':
          {
            str = str.substr(1);
            auto i = str.find('"');
            auto str2 = str.substr(0, i);
            str = str.substr(i + 1);
            return string{str2};
          }
        case 'f':
          if (str[1] == 'a' && str[2] == 'l' && str[3] == 's' && str[4] == 'e') {
            str = str.substr(5);
            return false;
          } else {
            throw parse_error{"Saw 'f', expected \"false\"", str};
          }
        case 't':
          if (str[1] == 'r' && str[2] == 'u' && str[3] == 'e') {
            str = str.substr(4);
            return true;
          } else {
            throw parse_error{"Saw 't', expected \"true\"", str};
          }
        case '[':
          {
            str = str.substr(1);
            auto arr = array{};
            while (true) {
              eat_whitespace(str);
              if (str[0] == ']') {
                str = str.substr(1);
                return arr;
              }
              arr.push_back(parse(str));
              eat_whitespace(str);
              switch (str[0]) {
                case ',':
                  str = str.substr(1);
                  break;
                case ']':
                  str = str.substr(1);
                  return arr;
                default:
                  throw parse_error{"Expected ',' or ']'", str};
              }
            }
          }
        case '{':
          {
            str = str.substr(1);
            auto obj = object{};
            while (true) {
              eat_whitespace(str);
              if (str[0] == '}') {
                str = str.substr(1);
                return obj;
              }
              auto field = [&] {
                if (str[0] == '"') {
                  str = str.substr(1);
                  auto i = str.find('"');
                  auto field = str.substr(0, i);
                  str = str.substr(i + 1);
                  return field;
                } else if (is_initial_ident_char(str[0])) {
                  auto field = str;
                  str = str.substr(1);
                  auto field_end = std::find_if_not(str.begin(), str.end(), is_ident_char);
                  field = field.substr(0, field_end - field.begin());
                  str = str.substr(field_end - str.begin());
                  return field;
                } else {
                  throw parse_error{"Expected a field name", str};
                }
              }();
              eat_whitespace(str);
              if (str[0] != ':') {
                throw parse_error{"Expected ':'", str};
              }
              str = str.substr(1);
              obj.emplace(field, parse(str));
              eat_whitespace(str);
              switch (str[0]) {
                case ',':
                  str = str.substr(1);
                  break;
                case '}':
                  str = str.substr(1);
                  return obj;
                default:
                  throw parse_error{"Expected ',' or '}'", str};
              }
            }
          }
        default:
          throw parse_error{"Unexpected character", str};
      }
    }
  }

  constexpr auto parse(std::string_view str) -> json::value {
    auto res = impl::parse(str);
    impl::eat_whitespace(str);
    if (!str.empty()) {
      throw parse_error{"Finished parsing but haven't reached the end of the string", str};
    }
    return res;
  }

  namespace literals {
    constexpr auto operator""_json(char const * str, std::size_t len) {
      return parse({str, len});
    }
  }

  struct schema {
    struct rec { string_constexpr ident; };

    struct any {};
    struct null {};
    struct _int {};
    struct _float {};
    struct string {};
    struct _bool {};

    struct array {
      unique_ptr_constexpr<schema> elements;

      friend constexpr void swap(array& lhs, array& rhs) {
        std::swap(lhs.elements, rhs.elements);
      }

      constexpr array(auto&& ...args) : elements{make_unique_constexpr<schema>(std::forward<decltype(args)>(args)...)} {}

      constexpr array(array const & that) : elements{make_unique_constexpr<schema>(*that.elements)} {}

      constexpr array& operator=(array const & that) {
        auto tmp = that;
        swap(*this, tmp);
        return *this;
      }

      constexpr array(array&&) = default;
      constexpr array& operator=(array&&) = default;
    };

    struct dict {
      unique_ptr_constexpr<schema> elements;

      friend constexpr void swap(dict& lhs, dict& rhs) {
        std::swap(lhs.elements, rhs.elements);
      }

      constexpr dict(auto&& ...args) : elements{make_unique_constexpr<schema>(std::forward<decltype(args)>(args)...)} {}

      constexpr dict(dict const & that) : elements{make_unique_constexpr<schema>(*that.elements)} {}

      constexpr dict& operator=(dict const & that) {
        auto tmp = that;
        swap(*this, tmp);
        return *this;
      }

      constexpr dict(dict&&) = default;
      constexpr dict& operator=(dict&&) = default;
    };

    struct object : trie<schema> { using trie<schema>::trie; };
    struct discriminated_union { string_constexpr discriminator; trie<object> cases; };

    using variant_type = variant_constexpr
      <any, null, _int, _float, string, _bool, array, dict, object, discriminated_union>;

    variant_type data;

    constexpr schema(schema&) = default;
    constexpr schema& operator=(schema&) = default;
    constexpr schema(schema const &) = default;
    constexpr schema& operator=(schema const &) = default;
    constexpr schema(schema&&) = default;
    constexpr schema& operator=(schema&&) = default;

    constexpr schema(auto&& ...args) : data{std::forward<decltype(args)>(args)...} {}
  };

  namespace typed {
    struct parsed_wrong_type : std::runtime_error { using std::runtime_error::runtime_error; };

    template <typename>
    struct parse_t;

    template <typename T> constexpr auto parse(json::value_const_view val) { return parse_t<T>{}(val); }

    template <typename>
    struct to_schema_t;

    template <typename T> constexpr auto to_schema() -> schema { return to_schema_t<T>::_schema(); }
    
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

    template <impl::is_context context, typename T> using in_context = typename in_context_t<T>::template type<context>;

    template <impl::is_decl ...Decls> struct context : private Decls... {
      private:
        using Decls::_lookup...;

      public:
        template <string_literal Ident> using lookup = in_context<context, decltype(_lookup(lift_to_type<Ident>{}))>;
    };

    namespace impl {
      template <is_decl ...Decls> constexpr auto _is_context<context<Decls...>> = true;
    }

    template <impl::is_context Context, string_literal Ident>
    class lazy {
      private:
        using type = typename Context::template lookup<Ident>;

        unique_ptr_constexpr<type> datum;

        constexpr lazy(unique_ptr_constexpr<type> datum) : datum{std::move(datum)} {}

      public:
        constexpr operator type       &  ()       &  { return *datum; }
        constexpr operator type const &  () const &  { return *datum; }
        constexpr operator type       && ()       && { return *std::move(datum); }

        constexpr auto operator*()       &  -> type       &  { return *datum; }
        constexpr auto operator*() const &  -> type const &  { return *datum; }
        constexpr auto operator*()       && -> type       && { return *std::move(datum); }
    
        constexpr auto operator->()       -> auto       & { return datum; }
        constexpr auto operator->() const -> auto const & { return datum; }

        friend struct parse_t<lazy<Context, Ident>>;
        friend struct to_schema_t<lazy<Context, Ident>>;
    };

    template <impl::is_context Context, string_literal Ident>
    struct parse_t<lazy<Context, Ident>> {
      constexpr auto operator()(json::value_const_view val) const -> lazy<Context, Ident> {
        using type = typename lazy<Context, Ident>::type;
        return make_unique_constexpr<type>(parse_t<type>{}(val));
      }
    };

    template <impl::is_context Context, string_literal Ident>
    constexpr auto untyped(lazy<Context, Ident> const & val) -> json::value_const_view { return untyped(*val); }
    template <impl::is_context Context, string_literal Ident>
    constexpr auto untyped(lazy<Context, Ident> &&      val) -> json::value { return untyped(*std::move(val)); }

    template <impl::is_context Context, string_literal Ident>
    struct to_schema_t<lazy<Context, Ident>> {
      static constexpr auto _schema() { return to_schema<typename lazy<Context, Ident>::type>(); }
    };

    template <string_literal Ident>
    struct rec;

    template <string_literal Ident>
    struct in_context_t<rec<Ident>> {
      template <impl::is_context Context> using type = lazy<Context, Ident>;
    };

    template <>
    struct parse_t<json::value> {
      constexpr auto operator()(json::value_const_view val) const -> json::value {
        return val;
      }
    };

    constexpr auto untyped(json::value const & val) -> json::value const & { return val; }
    constexpr auto untyped(json::value &&      val) -> json::value &&      { return std::move(val); }

    template <> struct to_schema_t<json::value> { static constexpr auto _schema() { return schema::any{}; } };

    template <> struct in_context_t<json::value> { template <impl::is_context> using type = json::value; };

    template <>
    struct parse_t<json::null> {
      constexpr auto operator()(json::value_const_view val) const -> json::null {
        if (val.is_null()) {
          return {};
        } else {
          throw parsed_wrong_type{"Expected a null, got "s + std::string{val.debug_type()}};
        }
      }
    };

    constexpr auto untyped(json::null val) -> json::null { return val; }

    template <> struct to_schema_t<json::null> { static constexpr auto _schema() { return schema::null{}; } };

    template <>
    struct in_context_t<json::null> {
      template <impl::is_context> using type = json::null;
    };

    template <>
    struct parse_t<long> {
      constexpr auto operator()(json::value_const_view val) const -> long {
        if (auto x = val.as_int()) {
          return *x;
        } else {
          throw parsed_wrong_type{"Expected an integer, got "s + std::string{val.debug_type()}};
        }
      }
    };

    constexpr auto untyped(long x) { return x; }

    template <> struct to_schema_t<long> { static constexpr auto _schema() { return schema::_int{}; } };

    template <> struct in_context_t<long> { template <impl::is_context> using type = long; };

    template <>
    struct parse_t<double> {
      constexpr auto operator()(json::value_const_view val) const -> double {
        if (auto x = val.as_float()) {
          return *x;
        } else {
          throw parsed_wrong_type{"Expected a float, got "s + std::string{val.debug_type()}};
        }
      }
    };

    constexpr auto untyped(double x) { return x; }

    template <> struct to_schema_t<double> { static constexpr auto _schema() { return schema::_float{}; } };

    template <>
    struct in_context_t<double> {
      template <impl::is_context> using type = double;
    };

    template <>
    struct parse_t<string_constexpr> {
      constexpr auto operator()(json::value_const_view val) const -> string_constexpr {
        if (auto str = val.as_string()) {
          return *str;
        } else {
          throw parsed_wrong_type{"Expected a string, got "s + std::string{val.debug_type()}};
        }
      }
    };

    constexpr auto untyped(string_constexpr const & str) -> string_constexpr const & { return str; }
    constexpr auto untyped(string_constexpr&& str) -> string_constexpr&& { return std::move(str); }

    template <> struct to_schema_t<string_constexpr> { static constexpr auto _schema() { return schema::string{}; } };

    template <> struct in_context_t<string_constexpr> { template <impl::is_context> using type = string_constexpr; };

    template <>
    struct parse_t<bool> {
      constexpr auto operator()(json::value_const_view val) const -> bool {
        if (auto x = val.as_bool()) {
          return *x;
        } else {
          throw parsed_wrong_type{"Expected a bool, got "s + std::string{val.debug_type()}};
        }
      }
    };

    constexpr auto untyped(bool x) { return x; }

    template <> struct to_schema_t<bool> { static constexpr auto _schema() { return schema::_bool{}; } };

    template <>
    struct in_context_t<bool> {
      template <impl::is_context> using type = bool;
    };

    template <typename T>
    struct parse_t<vector_constexpr<T>> {
      constexpr auto operator()(json::value_const_view val) const -> vector_constexpr<T> {
        if (auto xs = val.as_array()) {
          auto ys = vector_constexpr<T>(xs->size());
          std::transform(xs->begin(), xs->end(), polyfill::back_insert_iterator{ys}, parse_t<T>{});
          return ys;
        } else {
          throw parsed_wrong_type{"Expected an array, got "s + std::string{val.debug_type()}};
        }
      }
    };

    template <typename T> constexpr auto untyped(vector_constexpr<T> const & arr) -> json::array {
      auto res = json::array{};
      std::transform(arr.begin(), arr.end(), polyfill::back_insert_iterator{res}, [](auto&& x) { return untyped(std::forward<decltype(x)>(x)); });
      return res;
    }

    template <typename T> constexpr auto untyped(vector_constexpr<T>&& arr) -> json::array {
      auto res = json::array{};
      std::transform
        ( std::move_iterator{arr.begin()}
        , std::move_iterator{arr.end()}
        , polyfill::back_insert_iterator{res}
        , [](auto&& x) { return untyped(std::forward<decltype(x)>(x)); }
        );
      return res;
    }

    template <typename T> struct to_schema_t<vector_constexpr<T>> { static constexpr auto _schema() { return schema::array{to_schema<T>()}; } };

    template <typename T>
    struct in_context_t<vector_constexpr<T>> {
      template <impl::is_context Context> using type = vector_constexpr<in_context<Context, T>>;
    };

    template <typename T>
    struct parse_t<trie<T>> {
      constexpr auto operator()(json::value_const_view val) const -> trie<T> {
        if (auto xs = val.as_object()) {
          return trie<T>{parse_t<T>{}, *xs};
        } else {
          throw parsed_wrong_type{"Expected an array, got "s + std::string{val.debug_type()}};
        }
      }
    };

    template <typename T> constexpr auto untyped(trie<T> x) -> json::object { return x; }

    template <typename T> struct to_schema_t<trie<T>> { static constexpr auto _schema() { return schema::dict{to_schema<T>()}; } };

    template <typename T>
    struct in_context_t<trie<T>> {
      template <impl::is_context Context> using type = trie<in_context<Context, T>>;
    };

    template <string_literal Name, typename T>
    struct field {
      private:
        T datum;

      public:
        static constexpr auto name = Name;
        using type = T;
  
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

    template <string_literal Name, typename T>
    struct rec_field;

    template <string_literal Name, typename T>
    struct in_context_t<rec_field<Name, T>> {
      template <impl::is_context Context> using type = field<Name, in_context<Context, T>>;
    };

    namespace impl {
      template <typename>
      constexpr auto _is_field = false;

      template <string_literal Name, typename T>
      constexpr auto _is_field<field<Name, T>> = true;

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
    };

    template <impl::is_field ...Fields>
    struct parse_t<object<Fields...>> {
      constexpr auto operator()(json::value_const_view val) const -> object<Fields...> {
        if (auto obj = val.as_object()) {
          return object<Fields...>{*obj};
        } else {
          throw parsed_wrong_type{"Expected an object"};
        }
      }
    };
  
    template <impl::is_field ...Fields>
    constexpr auto untyped(object<Fields...> const & obj) {
      return json::object{std::pair{Fields::tag, obj.template get<Fields::tag>().untyped()}...};
    }
        
    template <impl::is_field ...Fields>
    constexpr auto untyped(object<Fields...> && obj) {
      return json::object{std::pair{Fields::tag, std::move(obj).template get<Fields::tag>().untyped()}...};
    }

    template <impl::is_field ...Fields>
    struct to_schema_t<object<Fields...>> {
      static constexpr auto _schema() {
        return schema::object{std::pair{Fields::name.view(), schema{to_schema<typename Fields::type>()}}...};
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

        constexpr get_match_case(MatchCases ...cases) : MatchCases{cases}... {}
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

            using func_ptr = auto (*)(char const *, json::object const &) -> discriminated_union<Discriminator, Cases...>;
  
            template <char c>
            static constexpr func_ptr advance_then_lookup = tag_trie<typename CaseConstructors::template advance<c>...>::lookup;
  
          public:
            static constexpr auto lookup(char const * tag, json::object const & x) -> discriminated_union<Discriminator, Cases...> {
              if constexpr ((CaseConstructors::done && ... && true)) {
                throw parsed_wrong_type{"Invalid tag value"};
              } else {
                constexpr auto fs = std::array<func_ptr, 1 + sizeof...(cs)>{static_cast<func_ptr>(get), advance_then_lookup<cs>...};
                return fs[static_cast<unsigned char>(*tag)](tag + 1, x);
              }
            }
        };

      public:
        constexpr auto operator()(json::value_const_view val) const -> discriminated_union<Discriminator, Cases...> {
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
    struct to_schema_t<discriminated_union<Discriminator, Cases...>> {
      static constexpr auto _schema() {
        return schema::discriminated_union
          { Discriminator.view()
          , { [] <string_literal Tag, impl::is_field ...Fields> (proxy<union_case<Tag, Fields...>>) {
                return std::pair
                  { Tag
                  , to_schema_t<object<Fields...>>::_schema()
                  };
              }(proxy<Cases>{})...
            }
          };
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

    namespace impl {
      template <auto const & start_it, auto const & end_it, auto const & ...existing>
      constexpr auto expand_iterators() {
        if constexpr (start_it == end_it) {
          return lift_to_type<&existing...>{};
        } else {
          auto next_it = start_it;
          ++next_it;
          return expand_iterators<next_it, end_it, existing..., *start_it>();
        }
      };

      template <typename Type, string_literal Rest>
      struct parse_result {
        using type = Type;
        static constexpr auto rest = Rest;
      };

      template <auto Value, string_literal Rest>
      struct parse_non_type_result {
        static constexpr auto value = Value;
        static constexpr auto rest = Rest;
      };

      template <string_literal Rest, typename ...>
      struct parse_fields_result {
        static constexpr auto rest = Rest;
      };

      template <string_literal Schema, string_literal Token>
      constexpr auto parse_token() {
        constexpr auto schema = trim_leading<Schema>();
        if constexpr (schema.starts_with(Token)) {
          return schema.template substr<Token.size>();
        } else {
          static_assert(always<false, lift_to_type<Schema>>, "Parse error");
        }
      }

      template <string_literal Schema>
      constexpr auto parse_ident() {
        constexpr auto schema = trim_leading<Schema>();
        if constexpr (json::impl::is_initial_ident_char(schema[0])) {
          constexpr auto ident_begin = Schema.begin() + Schema.find_first_non_whitespace();
          constexpr auto ident_end = std::find_if_not(ident_begin + 1, Schema.end(), json::impl::is_ident_char);
          constexpr auto ident = schema.template substr<0, ident_end - ident_begin>();
          constexpr auto schema1 = schema.template substr<ident_end - ident_begin>();
          return parse_non_type_result<ident, schema1>{};
        } else {
          static_assert(always<false, lift_to_type<Schema>>, "Parse error");
        }
      }

      template <string_literal Schema>
      constexpr auto parse_field_name() {
        constexpr auto schema = trim_leading<Schema>();
        if constexpr (schema.starts_with('"')) {
          constexpr auto schema1 = schema.template substr<1>();
          constexpr auto i = schema1.find('"');
          constexpr auto field = schema1.template substr<0, i>();
          constexpr auto schema2 = schema1.template substr<i + 1>();
          return parse_non_type_result<field, schema2>{};
        } else {
          return parse_ident<schema>();
        }
      }

      template <template <bool, string_literal, bool = false> typename parse_schema, bool is_rec, string_literal Schema, typename ...Accum>
      constexpr auto parse_fields() {
        if constexpr (Schema.starts_with('}')) {
          return parse_fields_result<Schema.template substr<1>(), Accum...>{};
        } else {
          using field_name = decltype(parse_field_name<Schema>());
          constexpr auto schema1 = parse_token<field_name::rest, ":">();
          using field_type = typename parse_schema<is_rec, schema1>::parse;
          using _field = std::conditional_t
            < is_rec
            , rec_field<field_name::value, typename field_type::type>
            , field<field_name::value, typename field_type::type>
            >;
          constexpr auto schema2 = trim_leading<field_type::rest>();
          if constexpr (schema2.starts_with(',')) {
            return parse_fields<parse_schema, is_rec, schema2.template substr<1>(), Accum..., _field>();
          } else if constexpr (schema2.starts_with('}')) {
            return parse_fields_result<schema2.template substr<1>(), Accum..., _field>{};
          } else {
            static_assert(always<false, lift_to_type<Schema>>, "Fields parse failed");
          }
        }
      }

      template <template <bool, string_literal, bool = false> typename parse_schema, bool is_rec, string_literal Schema, typename ...Accum>
      constexpr auto parse_cases() {
        if constexpr (Schema.starts_with('>')) {
          return parse_fields_result<Schema.template substr<1>(), Accum...>{};
        } else {
          using case_name = decltype(parse_field_name<Schema>());
          constexpr auto schema1 = parse_token<case_name::rest, ":">();
          constexpr auto schema2 = parse_token<schema1, "{">();

          constexpr auto _case = [] <string_literal Rest, typename ...Fields> (parse_fields_result<Rest, Fields...>) {
            if constexpr (is_rec) {
              return parse_result<rec_union_case<case_name::value, Fields...>, Rest>{};
            } else {
              return parse_result<union_case<case_name::value, Fields...>, Rest>{};
            }
          }(parse_fields<parse_schema, is_rec, schema2>());
          constexpr auto schema3 = trim_leading<decltype(_case)::rest>();
          if constexpr (schema3.starts_with(',')) {
            return parse_cases<parse_schema, is_rec, schema3.template substr<1>(), Accum..., typename decltype(_case)::type>();
          } else if constexpr (schema3.starts_with('>')) {
            return parse_fields_result<schema3.template substr<1>(), Accum..., typename decltype(_case)::type>{};
          } else {
            static_assert(always<false, lift_to_type<Schema>>, "Cases parse failed");
          }
        }
      }

      template <bool is_rec, string_literal Schema, bool force_consume_all = false>
      class parse_schema {
        private:
          static constexpr auto _parse() {
            constexpr auto schema = trim_leading<Schema>();

            if constexpr (schema.starts_with("rec")) {
              static_assert(is_rec, "Can only use rec in appropriate context");
              constexpr auto schema1 = parse_token<schema.template substr<3>(), "<">();
              using ident = decltype(parse_ident<schema1>());
              constexpr auto schema2 = parse_token<ident::rest, ">">();
              return parse_result<rec<ident::value>, schema2>{};
            } else if constexpr (schema.starts_with("any")) {
              return parse_result<value, schema.template substr<3>()>{};
            } else if constexpr (schema.starts_with("null")) {
              return parse_result<null, schema.template substr<4>()>{};
            } else if constexpr (schema.starts_with("int")) {
              return parse_result<long, schema.template substr<3>()>{};
            } else if constexpr (schema.starts_with("float")) {
              return parse_result<double, schema.template substr<5>()>{};
            } else if constexpr (schema.starts_with("string")) {
              return parse_result<string_constexpr, schema.template substr<6>()>{};
            } else if constexpr (schema.starts_with("bool")) {
              return parse_result<bool, schema.template substr<4>()>{};
            } else if constexpr (schema.starts_with("[")) {
              using elements = typename parse_schema<is_rec, schema.template substr<1>()>::parse;
              constexpr auto schema1 = parse_token<elements::rest, "]">();
              return parse_result<vector_constexpr<typename elements::type>, schema1>{};
            } else if constexpr (schema.starts_with("dict")) {
              constexpr auto schema1 = parse_token<schema.template substr<4>(), "<">();
              using elements = typename parse_schema<is_rec, schema1>::parse;
              constexpr auto schema2 = parse_token<elements::rest, ">">();
              return parse_result<trie<typename elements::type>, schema2>{};
            } else if constexpr (schema.starts_with("{")) {
              return [] <string_literal Rest, typename ...Fields> (parse_fields_result<Rest, Fields...>) {
                if constexpr (is_rec) {
                  return parse_result<rec_object<Fields...>, Rest>{};
                } else {
                  return parse_result<object<Fields...>, Rest>{};
                }
              }(parse_fields<parse_schema, is_rec, schema.template substr<1>()>());
            } else if constexpr (schema.starts_with("?")) {
              using discriminator = decltype(parse_field_name<schema.template substr<1>()>());
              constexpr auto schema1 = parse_token<discriminator::rest, "<">();
              return [&] <string_literal Rest, typename ...Cases> (parse_fields_result<Rest, Cases...>) {
                if constexpr (is_rec) {
                  return parse_result<rec_discriminated_union<discriminator::value, Cases...>, Rest>{};
                } else {
                  return parse_result<discriminated_union<discriminator::value, Cases...>, Rest>{};
                }
              }(parse_cases<parse_schema, is_rec, schema1>());
            } else {
              static_assert(always<false, lift_to_type<Schema>>, "schema parse failed");
            }
          }
  
        public:
          using parse = decltype(_parse());

          static_assert(!force_consume_all || trim_leading<parse::rest>().empty(), "Unexpected data after finished parsing");
      };

      template <string_literal Schema, typename ...Accum>
      constexpr auto parse_context() {
        constexpr auto schema = trim_leading<Schema>();
        if constexpr (schema.empty()) {
          return context<Accum...>{};
        } else {
          using ident = decltype(parse_ident<schema>());
          constexpr auto schema1 = parse_token<ident::rest, "=">();
          using type = typename parse_schema<true, schema1>::parse;
          using _decl = decl<ident::value, typename type::type>;
          return parse_context<type::rest, Accum..., _decl>();
        }
      }
    }

    template <string_literal Schema>
    using parse_schema = typename impl::parse_schema<false, Schema, true>::parse::type;

    template <string_literal Schema>
    using parse_context = decltype(impl::parse_context<Schema>());
  }
}

#endif
