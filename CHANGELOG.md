## 3.4.0.2

- Massaging bounds to support more GHC versions.

## 3.4.0

- Removed `adultDen` and fixed the level range for `highDen`.

## 3.3.0

- New `Unknown` level for Kanji above level `Two`
- `level` is now a total function
- Removed `averageLevel`, as it wasn't correct nor useful
- New `densities` function with associated type `CharCat` for getting more
  interesting character density information

## 3.2.1

- `ToJSON` and `FromJSON` instances for `Kanji` derived via `Generic`
  instead of being hand-written.

## 3.2.0

- `averageLevel` isn't total, and so now returns in `Maybe`

## 3.1.0.1

- Performance improvements

## 3.1.0

- Overall simplification of library and reduction in boilerplate
  - `AsKanji` typeclass removed
  - `Rank` and `Level` are now just `Level`
  - More liberal use of `Map` and `Set`
- `nanq` renamed to `kanji` and moved inside same project

## 2.0.0

- New JSON-only output
- Revamped, modernized backend
