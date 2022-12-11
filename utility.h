#include <algorithm>
#include <array>
#include <cstddef>

template <typename T>
struct in_place_type_t {
  explicit in_place_type_t() = default;
};

template <typename T>
inline constexpr in_place_type_t<T> in_place_type{};

template <std::size_t I>
struct in_place_index_t {
  explicit in_place_index_t() = default;
};

template <std::size_t I>
inline constexpr in_place_index_t<I> in_place_index{};

namespace variant_details {
template <std::size_t I>
using index = std::integral_constant<std::size_t, I>;

template <std::size_t I, typename... Types>
using type_at_index = std::tuple_element_t<I, std::tuple<Types...>>;

template <typename T, typename... Types>
inline constexpr std::size_t type_index_v = [] {
  constexpr auto is_same = std::array{std::is_same_v<T, Types>...};
  return std::find(is_same.begin(), is_same.end(), true) - is_same.begin();
}();

template <typename T, template <typename...> typename Variant, typename... Types>
constexpr auto variant_type_index(const Variant<Types...>&) noexcept {
  return index<type_index_v<T, Types...>>{};
};

template <typename T, typename Variant>
inline constexpr std::size_t variant_type_index_v = decltype(variant_type_index<T>(std::declval<Variant>()))::value;

template <typename T, typename... Types>
inline constexpr bool exactly_one_v = (std::is_same_v<T, Types> + ...) == 1;

template <typename... Args>
constexpr auto make_array(Args&&... args) noexcept {
  return std::array<std::common_type_t<Args...>, sizeof...(Args)>{std::forward<Args>(args)...};
}

template <template <typename...> typename Variant, typename... Types>
constexpr auto variant_index_sequence(const Variant<Types...>&) noexcept {
  return std::make_index_sequence<sizeof...(Types)>{};
}

template <template <bool, typename...> typename Variant, bool flag, typename... Types>
constexpr auto variant_index_sequence(const Variant<flag, Types...>&) noexcept {
  return std::make_index_sequence<sizeof...(Types)>{};
}

template <typename Variant>
using variant_index_sequence_t = decltype(variant_index_sequence(std::declval<Variant>()));

template <typename... Types>
concept copy_constructible = (std::is_copy_constructible_v<Types> && ...);
template <typename... Types>
concept move_constructible = (std::is_move_constructible_v<Types> && ...);
template <typename... Types>
concept copy_assignable = copy_constructible<Types...> && (std::is_copy_assignable_v<Types> && ...);
template <typename... Types>
concept move_assignable = move_constructible<Types...> && (std::is_move_assignable_v<Types> && ...);

template <typename... Types>
concept trivially_destructible = (std::is_trivially_destructible_v<Types> && ...);
template <typename... Types>
concept trivially_copy_constructible =
    copy_constructible<Types...> && (std::is_trivially_copy_constructible_v<Types> && ...);
template <typename... Types>
concept trivially_move_constructible =
    move_constructible<Types...> && (std::is_trivially_move_constructible_v<Types> && ...);
template <typename... Types>
concept trivially_copy_assignable =
    copy_assignable<Types...> && trivially_copy_constructible<Types...> && trivially_destructible<Types...> &&
    (std::is_trivially_copy_assignable_v<Types> && ...);
template <typename... Types>
concept trivially_move_assignable =
    move_assignable<Types...> && trivially_move_constructible<Types...> && trivially_destructible<Types...> &&
    (std::is_trivially_move_assignable_v<Types> && ...);

template <typename T, typename T_i>
concept valid_overload = requires(T && t) {
  std::array<T_i, 1>{{std::forward<T>(t)}};
};

template <typename T>
struct is_in_place_tag : std::false_type {};
template <typename T>
struct is_in_place_tag<in_place_type_t<T>> : std::true_type {};
template <std::size_t I>
struct is_in_place_tag<in_place_index_t<I>> : std::true_type {};

template <typename T>
inline constexpr bool is_in_place_tag_v = is_in_place_tag<T>::value;
} // namespace variant_details
