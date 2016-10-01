module Data.MessagePack.IntBench (suite) where

import           Criterion.Main       (Benchmark, bench, bgroup, nf)
import qualified Data.ByteString.Lazy as LBS
import           Data.Int             (Int64)

import           Data.MessagePack


packInt :: Int64 -> LBS.ByteString
packInt = pack

unpackInt :: LBS.ByteString -> Maybe Int64
unpackInt = unpack


suite :: [Benchmark]
suite =
  [ bgroup "pack" -- should be constant time
    [ bench "0x1"                 $ nf packInt   0x1
    , bench "0x10"                $ nf packInt   0x10
    , bench "0x100"               $ nf packInt   0x100
    , bench "0x1000"              $ nf packInt   0x1000
    , bench "0x10000"             $ nf packInt   0x10000
    , bench "0x100000"            $ nf packInt   0x100000
    , bench "0x1000000"           $ nf packInt   0x1000000
    , bench "0x10000000"          $ nf packInt   0x10000000
    , bench "0x100000000"         $ nf packInt   0x100000000
    , bench "0x1000000000"        $ nf packInt   0x1000000000
    , bench "0x10000000000"       $ nf packInt   0x10000000000
    , bench "0x100000000000"      $ nf packInt   0x100000000000
    , bench "0x1000000000000"     $ nf packInt   0x1000000000000
    , bench "0x10000000000000"    $ nf packInt   0x10000000000000
    , bench "0x100000000000000"   $ nf packInt   0x100000000000000
    , bench "0x1000000000000000"  $ nf packInt   0x1000000000000000
    , bench "-0x1"                $ nf packInt (-0x1               )
    , bench "-0x10"               $ nf packInt (-0x10              )
    , bench "-0x100"              $ nf packInt (-0x100             )
    , bench "-0x1000"             $ nf packInt (-0x1000            )
    , bench "-0x10000"            $ nf packInt (-0x10000           )
    , bench "-0x100000"           $ nf packInt (-0x100000          )
    , bench "-0x1000000"          $ nf packInt (-0x1000000         )
    , bench "-0x10000000"         $ nf packInt (-0x10000000        )
    , bench "-0x100000000"        $ nf packInt (-0x100000000       )
    , bench "-0x1000000000"       $ nf packInt (-0x1000000000      )
    , bench "-0x10000000000"      $ nf packInt (-0x10000000000     )
    , bench "-0x100000000000"     $ nf packInt (-0x100000000000    )
    , bench "-0x1000000000000"    $ nf packInt (-0x1000000000000   )
    , bench "-0x10000000000000"   $ nf packInt (-0x10000000000000  )
    , bench "-0x100000000000000"  $ nf packInt (-0x100000000000000 )
    , bench "-0x1000000000000000" $ nf packInt (-0x1000000000000000)
    ]
  , bgroup "unpack" -- should be constant time
    [ bench "0x1"                 $ nf unpackInt (packInt   0x1                )
    , bench "0x10"                $ nf unpackInt (packInt   0x10               )
    , bench "0x100"               $ nf unpackInt (packInt   0x100              )
    , bench "0x1000"              $ nf unpackInt (packInt   0x1000             )
    , bench "0x10000"             $ nf unpackInt (packInt   0x10000            )
    , bench "0x100000"            $ nf unpackInt (packInt   0x100000           )
    , bench "0x1000000"           $ nf unpackInt (packInt   0x1000000          )
    , bench "0x10000000"          $ nf unpackInt (packInt   0x10000000         )
    , bench "0x100000000"         $ nf unpackInt (packInt   0x100000000        )
    , bench "0x1000000000"        $ nf unpackInt (packInt   0x1000000000       )
    , bench "0x10000000000"       $ nf unpackInt (packInt   0x10000000000      )
    , bench "0x100000000000"      $ nf unpackInt (packInt   0x100000000000     )
    , bench "0x1000000000000"     $ nf unpackInt (packInt   0x1000000000000    )
    , bench "0x10000000000000"    $ nf unpackInt (packInt   0x10000000000000   )
    , bench "0x100000000000000"   $ nf unpackInt (packInt   0x100000000000000  )
    , bench "0x1000000000000000"  $ nf unpackInt (packInt   0x1000000000000000 )
    , bench "-0x1"                $ nf unpackInt (packInt (-0x1               ))
    , bench "-0x10"               $ nf unpackInt (packInt (-0x10              ))
    , bench "-0x100"              $ nf unpackInt (packInt (-0x100             ))
    , bench "-0x1000"             $ nf unpackInt (packInt (-0x1000            ))
    , bench "-0x10000"            $ nf unpackInt (packInt (-0x10000           ))
    , bench "-0x100000"           $ nf unpackInt (packInt (-0x100000          ))
    , bench "-0x1000000"          $ nf unpackInt (packInt (-0x1000000         ))
    , bench "-0x10000000"         $ nf unpackInt (packInt (-0x10000000        ))
    , bench "-0x100000000"        $ nf unpackInt (packInt (-0x100000000       ))
    , bench "-0x1000000000"       $ nf unpackInt (packInt (-0x1000000000      ))
    , bench "-0x10000000000"      $ nf unpackInt (packInt (-0x10000000000     ))
    , bench "-0x100000000000"     $ nf unpackInt (packInt (-0x100000000000    ))
    , bench "-0x1000000000000"    $ nf unpackInt (packInt (-0x1000000000000   ))
    , bench "-0x10000000000000"   $ nf unpackInt (packInt (-0x10000000000000  ))
    , bench "-0x100000000000000"  $ nf unpackInt (packInt (-0x100000000000000 ))
    , bench "-0x1000000000000000" $ nf unpackInt (packInt (-0x1000000000000000))
    ]
  ]
