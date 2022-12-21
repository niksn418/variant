#include "utility.h"

namespace variant_details {
template <bool indexed, typename Visitor, typename IndexSequence, typename variant_index, typename... Variants>
struct table_t;

template <bool indexed, typename Visitor, std::size_t... type_indexes, typename... Variants>
struct table_t<indexed, Visitor, std::index_sequence<type_indexes...>, index<sizeof...(Variants)>, Variants...> {
private:
  static constexpr auto make_value() noexcept {
    if constexpr (indexed) {
      return [](Visitor&& visitor, Variants&&... variants) {
        return std::forward<Visitor>(visitor)(get_impl<type_indexes>(std::forward<Variants>(variants))...,
                                              index<type_indexes>{}...);
      };
    } else {
      return [](Visitor&& visitor, Variants&&... variants) {
        return std::forward<Visitor>(visitor)(get_impl<type_indexes>(std::forward<Variants>(variants))...);
      };
    }
  }

public:
  static constexpr auto value = make_value();
};

template <bool indexed, typename Visitor, std::size_t... type_indexes, std::size_t variant_index, typename... Variants>
struct table_t<indexed, Visitor, std::index_sequence<type_indexes...>, index<variant_index>, Variants...> {
private:
  using current_variant = std::remove_cvref_t<type_at_index<variant_index, Variants...>>;

  template <std::size_t... variant_type_indexes>
  static constexpr auto make_value(std::index_sequence<variant_type_indexes...>) noexcept {
    return make_array(table_t<indexed, Visitor, std::index_sequence<type_indexes..., variant_type_indexes>,
                              index<variant_index + 1>, Variants...>::value...);
  }

public:
  static constexpr auto value = make_value(variant_index_sequence_t<current_variant>{});
};

template <bool indexed, typename Visitor, typename... Variants>
using table = table_t<indexed, Visitor, std::index_sequence<>, index<0>, Variants...>;

template <typename Table>
constexpr auto table_get(const Table& table, std::size_t i) noexcept {
  return table[i];
}

template <typename Table, typename... Indexes>
constexpr auto table_get(const Table& table, std::size_t i, Indexes... indexes) noexcept {
  return table_get(table[i], indexes...);
}

template <bool indexed, typename Visitor, typename... Variants, typename... Indexes>
constexpr auto table_get(Indexes... indexes) noexcept {
  return table_get(table<indexed, Visitor, Variants...>::value, indexes...);
}

template <typename Visitor, typename... Variants>
constexpr decltype(auto) visit_impl(Visitor&& visitor, Variants&&... variants) {
  return table_get<false, Visitor&&, Variants&&...>(variants.index()...)(std::forward<Visitor>(visitor),
                                                                         std::forward<Variants>(variants)...);
}

template <typename Visitor, typename... Variants>
constexpr decltype(auto) indexed_visit(Visitor&& visitor, Variants&&... variants) {
  return table_get<true, Visitor&&, Variants&&...>(variants.index()...)(std::forward<Visitor>(visitor),
                                                                        std::forward<Variants>(variants)...);
}
} // namespace variant_details
