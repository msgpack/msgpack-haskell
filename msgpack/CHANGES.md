## 1.0.1.0

- Fix incorrect MessagePack tag when encoding single-precision `Float`s
- Fix looping/hanging `MessagePack (Maybe a)` instance
- Add support for `binary-0.8` API
- Drop dependency on `blaze-builder`
- Add new operations
   - `getWord`, `getWord64`, `getInt64`
   - `putWord`, `putWord64`, `putInt64`
- Add `Read` instance for `Object` and `Assoc`
- Add `Generic` instance for `Object`
- Add `Object` instance `ShortByteString`
- Declare API `Trustworthy` for SafeHaskell
