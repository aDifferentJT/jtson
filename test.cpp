
#include "jtson.hpp"

constexpr auto test() {
  using namespace json::literals;

  auto obj = json::object
    { std::pair{"tag", "foo"_jstr}
    , std::pair{"x", 42}
    , std::pair{"y", 10}
    , std::pair{"arr", json::array{3, 1, 4, 1, 5}}
    };

  auto typed_obj = json::typed::object
    < json::typed::field<"x", long>
    , json::typed::field<"y", json::value>
    , json::typed::field<"arr", vector_constexpr<long>>
    >{obj};

  auto typed_union = json::typed::discriminated_union<"tag", json::typed::union_case<"foo", json::typed::field<"x", long>>, json::typed::union_case<"bar", json::typed::field<"y", json::value>>>{json::typed::union_case<"foo", json::typed::field<"x", long>>{json::typed::field<"x", long>{42}}};

  auto typed_union2 = json::typed::parse<json::typed::discriminated_union<"tag", json::typed::union_case<"foo", json::typed::field<"x", long>>, json::typed::union_case<"bar", json::typed::field<"y", json::value>>>>(obj);

  typed_union.visit<void>
    ( [] (auto) {
      }
    );
  typed_union.match<void>
    ( json::typed::match_case<"foo">
      ([](json::typed::object<json::typed::field<"x", long>>) {})
    , json::typed::match_case<"bar">
      ([](json::typed::object<json::typed::field<"y", json::value>>) {})
    );

  return typed_obj.get<"x">();
}

int main() {
  using namespace json::literals;

  [[maybe_unused]] constexpr auto x = test();

  auto obj = json::object
    { std::pair{"tag", "foo"_jstr}
    , std::pair{"x", 42}
    , std::pair{"y", 10}
    , std::pair{"arr", json::array{3, 1, 4, 1, 5}}
    };

  auto typed_obj = json::typed::object
    < json::typed::field<"x", long>
    , json::typed::field<"y", json::value>
    , json::typed::field<"arr", vector_constexpr<long>>
    >{obj};

  std::cout << obj << '\n';

  auto typed_union = json::typed::discriminated_union<"tag", json::typed::union_case<"foo", json::typed::field<"x", long>>, json::typed::union_case<"bar", json::typed::field<"y", json::value>>>{json::typed::union_case<"foo", json::typed::field<"x", long>>{json::typed::field<"x", long>{42}}};

  auto typed_union2 = json::typed::parse<json::typed::discriminated_union<"tag", json::typed::union_case<"foo", json::typed::field<"x", long>>, json::typed::union_case<"bar", json::typed::field<"y", json::value>>>>(obj);

  typed_union.visit<void>
    ( [] (auto) {
        std::cout << "foo\n";
      }
    );
  typed_union.match<void>
    ( json::typed::match_case<"foo">
      ( [] (json::typed::object<json::typed::field<"x", long>> x) {
          std::cout << "int: " << x.get<"x">() << '\n';
        }
      )
    , json::typed::match_case<"bar">
      ( [] (json::typed::object<json::typed::field<"y", json::value>> x) {
          std::cout << "string: " << x.get<"y">() << '\n';
        }
      )
    );

  std::cout << untyped(typed_union2) << '\n';

  auto string = std::string{"foo"_jstr};

  auto typed_rec =
    json::typed::parse
      < json::typed::context
        < json::typed::decl
          < "obj"
          , json::typed::rec_discriminated_union
            < "tag"
            , json::typed::rec_union_case<"nil">
            , json::typed::rec_union_case
              < "cons"
              , json::typed::field<"x", long>
              , json::typed::rec_field<"xs", "obj">
              >
            >
          >
        >::lookup<"obj">
      >
      ( json::object
        { std::pair{"tag", "cons"}
        , std::pair{"x", 3}
        , std::pair
          { "xs"
          , json::object
            { std::pair{"tag", "cons"}
            , std::pair{"x", 1}
            , std::pair
              { "xs"
              , json::object
                { std::pair{"tag", "cons"}
                , std::pair{"x", 4}
                , std::pair
                  { "xs"
                  , json::object
                    { std::pair{"tag", "cons"}
                    , std::pair{"x", 1}
                    , std::pair
                      { "xs"
                      , json::object
                        { std::pair{"tag", "cons"}
                        , std::pair{"x", 5}
                        , std::pair
                          { "xs"
                          , json::object{std::pair{"tag", "nil"}}
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      );

  namespace vldtr = json::validator;
  auto validator = vldtr::any_validator{vldtr::object<vldtr::field<"x", vldtr::any>>{}};
  auto validator2 = validator;
  auto validator3 = std::move(validator2);
  return validator3(obj);
}

