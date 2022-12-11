#pragma once

#include "visit_table.h"
#include <array>

template <typename... Types>
class variant;

template <typename T>
struct variant_size;

template <typename... Types>
struct variant_size<variant<Types...>> : std::integral_constant<std::size_t, sizeof...(Types)> {};

template <typename T>
struct variant_size<const T> : variant_size<T> {};

template <typename T>
inline constexpr std::size_t variant_size_v = variant_size<T>::value;

template <std::size_t I, typename T>
struct variant_alternative;

template <std::size_t I, typename... Types>
struct variant_alternative<I, variant<Types...>> {
  using type = variant_details::type_at_index<I, Types...>;
};

template <std::size_t I, typename T>
using variant_alternative_t = typename variant_alternative<I, T>::type;

template <std::size_t I, typename T>
struct variant_alternative<I, const T> {
  using type = std::add_const_t<variant_alternative_t<I, T>>;
};

inline constexpr std::size_t variant_npos = -1;

struct bad_variant_access : std::exception {
  const char* what() const noexcept override {
    return "bad variant access";
  }
};

template <typename T, typename... Types>
constexpr bool holds_alternative(const variant<Types...>& v) noexcept {
  return v.index() == variant_details::type_index_v<T, Types...>;
}

namespace variant_details {
template <typename...>
union storage_t {};

template <typename T, typename... Types>
union storage_t<T, Types...> {
  T value;
  storage_t<Types...> others;

  constexpr storage_t() noexcept : others() {}
  ~storage_t() noexcept {}
};

template <trivially_destructible T, trivially_destructible... Types>
union storage_t<T, Types...> {
  T value;
  storage_t<Types...> others;

  constexpr storage_t() noexcept : others() {}
  ~storage_t() = default;
};

template <std::size_t I>
constexpr auto* get_storage(auto* storage) noexcept {
  if constexpr (I == 0) {
    return storage;
  } else {
    return get_storage<I - 1>(&storage->others);
  }
}

template <bool trivial_destruct = true, typename... Types>
struct destruct_base {
  storage_t<Types...> storage;
  std::size_t idx = variant_npos;

  template <std::size_t I>
  constexpr auto& get() & noexcept {
    return get_storage<I>(&storage)->value;
  }

  template <std::size_t I>
  constexpr auto&& get() && noexcept {
    return std::move(get<I>());
  }

  template <std::size_t I>
  constexpr const auto& get() const& noexcept {
    return get_storage<I>(&storage)->value;
  }

  template <std::size_t I>
  constexpr const auto&& get() const&& noexcept {
    return std::move(get<I>());
  }

  constexpr std::size_t index() const noexcept {
    return idx;
  }

  constexpr void destruct() noexcept {
    idx = variant_npos;
  }

  ~destruct_base() = default;
};

template <typename... Types>
struct destruct_base<false, Types...> : destruct_base<true, Types...> {
  constexpr void destruct() {
    if (this->idx != variant_npos) {
      visit_impl([](auto& value) { std::destroy_at(std::addressof(value)); }, *this);
      this->idx = variant_npos;
    }
  }

  ~destruct_base() {
    destruct();
  }
};

template <std::size_t I, typename Variant>
constexpr decltype(auto) get_impl(Variant&& v) noexcept {
  return std::forward<Variant>(v).template get<I>();
}

template <std::size_t I>
constexpr auto* get_if_impl(auto* v) noexcept {
  return std::addressof(get_impl<I>(*v));
}

template <typename... Types>
using variant_destruct_base = destruct_base<trivially_destructible<Types...>, Types...>;

template <typename T, typename T_i, std::size_t I>
struct overload {
  static constexpr void get() noexcept;
};

template <typename T, typename T_i, std::size_t I>
requires(valid_overload<T, T_i>) struct overload<T, T_i, I> {
  static constexpr T_i get(T_i) noexcept;
};

template <typename T, typename Variant, typename = variant_index_sequence_t<Variant>>
struct overload_chooser;

template <typename T, typename... Types, std::size_t... Indexes>
struct overload_chooser<T, variant<Types...>, std::index_sequence<Indexes...>> : overload<T, Types, Indexes>... {
  using overload<T, Types, Indexes>::get...;
};

template <typename T, typename Variant>
using choose_overload = decltype(overload_chooser<T, Variant>::get(std::declval<T>()));

template <typename Comp, typename Variant>
constexpr bool compare(const Variant& lhs, const Variant& rhs, Comp comp = Comp()) {
  if (lhs.index() == rhs.index() && !lhs.valueless_by_exception()) {
    return visit_impl(
        [&comp](const auto& lhs, const auto& rhs) {
          if constexpr (std::is_same_v<decltype(lhs), decltype(rhs)>) {
            return comp(lhs, rhs);
          }
          return false;
        },
        lhs, rhs);
  } else {
    return comp(lhs.index() + 1, rhs.index() + 1);
  }
}
} // namespace variant_details

template <typename... Types>
class variant : variant_details::variant_destruct_base<Types...> {
  template <std::size_t I, typename Variant>
  friend constexpr decltype(auto) variant_details::get_impl(Variant&& v) noexcept;

public:
  constexpr variant() noexcept(std::is_nothrow_default_constructible_v<variant_details::type_at_index<0, Types...>>)
      requires(std::is_default_constructible_v<variant_details::type_at_index<0, Types...>>)
      : variant(in_place_index<0>) {}

  variant(const variant& other) = delete;
  variant(const variant& other) requires(variant_details::trivially_copy_constructible<Types...>) = default;
  variant(const variant& other) requires(variant_details::copy_constructible<Types...>) {
    if (!other.valueless_by_exception()) {
      variant_details::indexed_visit(
          [this](const auto& other_value, auto other_index) { this->template emplace<other_index>(other_value); },
          other);
    }
  }

  variant(variant&&) = delete;
  variant(variant&&) requires(variant_details::trivially_move_constructible<Types...>) = default;
  constexpr variant(variant&& other) noexcept((std::is_nothrow_move_constructible_v<Types> && ...))
      requires(variant_details::move_constructible<Types...>) {
    if (!other.valueless_by_exception()) {
      variant_details::indexed_visit(
          [this](auto&& other_value, auto other_index) { this->template emplace<other_index>(std::move(other_value)); },
          std::move(other));
    }
  }

  variant& operator=(const variant&) = delete;
  variant& operator=(const variant&) requires(variant_details::trivially_copy_assignable<Types...>) = default;
  constexpr variant& operator=(const variant& other) requires(variant_details::copy_assignable<Types...>) {
    if (other.valueless_by_exception()) {
      this->destruct();
    } else {
      variant_details::indexed_visit(
          [this, &other](const auto& other_value, auto other_index) {
            if (index() == other_index) {
              variant_details::get_impl<other_index>(*this) = other_value;
            } else if constexpr (std::is_nothrow_copy_constructible_v<std::decay_t<decltype(other_value)>> ||
                                 std::is_nothrow_move_constructible_v<std::decay_t<decltype(other_value)>>) {
              this->template emplace<other_index>(other_value);
            } else {
              *this = variant(other);
            }
          },
          other);
    }
    return *this;
  }

  variant& operator=(variant&&) = delete;
  variant& operator=(variant&&) requires(variant_details::trivially_move_assignable<Types...>) = default;
  constexpr variant& operator=(variant&& other) noexcept(((std::is_nothrow_move_constructible_v<Types> &&
                                                           std::is_nothrow_move_assignable_v<Types>)&&...))
      requires(variant_details::move_assignable<Types...>) {
    if (other.valueless_by_exception()) {
      this->destruct();
    } else {
      variant_details::indexed_visit(
          [this](auto&& other_value, auto other_index) {
            if (index() == other_index) {
              variant_details::get_impl<other_index>(*this) = std::move(other_value);
            } else {
              this->template emplace<other_index>(std::move(other_value));
            }
          },
          std::move(other));
    }
    return *this;
  }

  template <typename T, typename T_i = variant_details::choose_overload<T, variant>>
  requires(sizeof...(Types) > 0 && !std::is_same_v<std::remove_cvref_t<T>, variant> &&
           !variant_details::is_in_place_tag_v<std::remove_cvref_t<T>> && std::is_constructible_v<T_i, T> &&
           variant_details::exactly_one_v<
               T_i, Types...>) constexpr variant(T&& t) noexcept(std::is_nothrow_constructible_v<T_i, T>)
      : variant(in_place_type<T_i>, std::forward<T>(t)) {}

  template <typename T, typename T_i = variant_details::choose_overload<T, variant>>
  requires(!std::is_same_v<std::remove_cvref_t<T>, variant> && std::is_assignable_v<T_i&, T> &&
           std::is_constructible_v<T_i, T> && variant_details::exactly_one_v<T_i, Types...>) constexpr variant&
  operator=(T&& t) noexcept(std::is_nothrow_assignable_v<T_i&, T>&& std::is_nothrow_constructible_v<T_i, T>) {
    if (holds_alternative<T_i>(*this)) {
      variant_details::get_impl<T_i>(*this) = std::forward<T>(t);
    } else if constexpr (std::is_nothrow_constructible_v<T_i, T> || !std::is_nothrow_move_constructible_v<T_i>) {
      emplace<T_i>(std::forward<T>(t));
    } else {
      emplace<T_i>(T_i(std::forward<T>(t)));
    }
    return *this;
  }

  template <typename T, typename... Args>
  requires(std::is_constructible_v<T, Args...>&&
               variant_details::exactly_one_v<T, Types...>) constexpr explicit variant(in_place_type_t<T>,
                                                                                       Args&&... args) {
    emplace<T>(std::forward<Args>(args)...);
  }

  template <std::size_t I, typename... Args>
  requires(I < sizeof...(Types) && std::is_constructible_v<variant_details::type_at_index<I, Types...>,
                                                           Args...>) constexpr explicit variant(in_place_index_t<I>,
                                                                                                Args&&... args) {
    emplace<I>(std::forward<Args>(args)...);
  }

  constexpr std::size_t index() const noexcept {
    return variant_details::variant_destruct_base<Types...>::index();
  }

  constexpr bool valueless_by_exception() const noexcept {
    return index() == variant_npos;
  }

  template <typename T, typename... Args>
  requires(std::is_constructible_v<T, Args...>&& variant_details::exactly_one_v<T, Types...>) constexpr T& emplace(
      Args&&... args) {
    return emplace<variant_details::type_index_v<T, Types...>>(std::forward<Args>(args)...);
  }

  template <std::size_t I, typename... Args>
  requires(I < sizeof...(Types) &&
           std::is_constructible_v<variant_details::type_at_index<I, Types...>,
                                   Args...>) constexpr variant_alternative_t<I, variant>& emplace(Args&&... args) {
    this->destruct();
    auto& result = *std::construct_at(variant_details::get_if_impl<I>(this), std::forward<Args>(args)...);
    this->idx = I;
    return result;
  }

  constexpr void swap(variant& other) noexcept(((std::is_nothrow_move_constructible_v<Types> &&
                                                 std::is_nothrow_swappable_v<Types>)&&...)) {
    if (valueless_by_exception() && other.valueless_by_exception()) {
      return;
    }
    if (index() == other.index()) {
      variant_details::visit_impl(
          [](auto& this_value, auto& other_value) {
            if constexpr (std::is_same_v<decltype(this_value), decltype(other_value)>) {
              using std::swap;
              swap(this_value, other_value);
            }
          },
          *this, other);
    } else {
      *this = std::exchange(other, std::move(*this));
    }
  }

  friend constexpr bool operator==(const variant& lhs, const variant& rhs) {
    return variant_details::compare<std::equal_to<void>>(lhs, rhs);
  }

  friend constexpr bool operator!=(const variant& lhs, const variant& rhs) {
    return variant_details::compare<std::not_equal_to<void>>(lhs, rhs);
  }

  friend constexpr bool operator<(const variant& lhs, const variant& rhs) {
    return variant_details::compare<std::less<void>>(lhs, rhs);
  }

  friend constexpr bool operator>(const variant& lhs, const variant& rhs) {
    return variant_details::compare<std::greater<void>>(lhs, rhs);
  }

  friend constexpr bool operator<=(const variant& lhs, const variant& rhs) {
    return variant_details::compare<std::less_equal<void>>(lhs, rhs);
  }

  friend constexpr bool operator>=(const variant& lhs, const variant& rhs) {
    return variant_details::compare<std::greater_equal<void>>(lhs, rhs);
  }
};

template <std::size_t I, typename Variant>
constexpr decltype(auto) get(Variant&& v) requires(I < variant_size_v<std::remove_cvref_t<Variant>>) {
  if (v.index() == I) {
    return variant_details::get_impl<I>(std::forward<Variant>(v));
  }
  throw bad_variant_access{};
}

template <typename T, typename Variant>
constexpr decltype(auto) get(Variant&& v) {
  return get<variant_details::variant_type_index_v<T, Variant>>(std::forward<Variant>(v));
}

template <std::size_t I, typename Variant>
constexpr std::add_pointer_t<variant_alternative_t<I, Variant>> get_if(Variant* v) noexcept {
  if (v != nullptr && v->index() == I) {
    return variant_details::get_if_impl<I>(v);
  }
  return nullptr;
}

template <typename T, typename Variant>
constexpr decltype(auto) get_if(Variant* v) noexcept {
  return get_if<variant_details::variant_type_index_v<T, Variant>>(v);
}

template <typename Visitor, typename... Variants>
constexpr decltype(auto) visit(Visitor&& visitor, Variants&&... vars) {
  if ((vars.valueless_by_exception() || ...)) {
    throw bad_variant_access();
  }
  return variant_details::visit_impl(std::forward<Visitor>(visitor), std::forward<Variants>(vars)...);
}
