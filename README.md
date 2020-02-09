# hledger-exporter

This repository contains an example management script for use with
[hledger](https://github.com/simonmichael/hledger).

It is based of the `export.hs` script from [adept](https://github.com/adept)'s
[full-fledged-hledger](https://github.com/adept/full-fledged-hledger)
repository.

This will generate a list of year reports in a `reports` directory, a list of
closing entries in `closing`, & a list of opening entries in `opening`. It will
search for includes pointing to the `import/` directory and generate the
journal files according to the `full-fledged-hledger` repository.

To customize the script:

* Change the `ImportAccount` type to accounts that should be processed and
  update the `importAccountFolder` function.
* Set the `firstYear` & `currentYear` variables to their proper values.
* Set the `openingAccount`, `closingAccount`, & `openCloseQuery` to the values
  you desire.

To use this in your project, copy `src/Main.hs` into the root of your hledger
directory and add a stack script interpreter to the top of the file:

```
#!/usr/bin/env stack
{- stack script
    --resolver lts-14.23

    --ghc-options -Wall
    --ghc-options -Werror
    --ghc-options -threaded
    --ghc-options -with-rtsopts=-N

    --package hashable
    --package shake
-}
```


## License

BSD-3
