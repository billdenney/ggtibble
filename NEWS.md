# ggtibble 1.0.1.9000

* `ggtibble()` now warns if `outercols` are not used in either the `caption` or
  the `labs` argument (#13).
* `ggtibble` and `gglist` objects now work with the ggplot2 `%+%` operator (#16)
* A new `ggsave()` generic function will now enable simpler saving of `ggtibble`
  and `gglist` objects.

# ggtibble 1.0.1

* `labs` argument to `ggtibble()` can now include `NULL (#6)
* `guides()` can now be added to `gglist` objects.
* Labels created with the `labs` argument to `ggtibble()` will not longer all be
  the same (#3)
* `new_gglist()` and `new_ggtibble()` are now exported making it easier to
  create objects.
