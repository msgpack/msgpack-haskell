-- | Common Prelude-ish module
module Compat.Prelude
    ( module X
    ) where

import           Control.Applicative as X
import           Control.DeepSeq     as X (NFData (rnf))
import           Control.Monad       as X
import           Data.Bits           as X (complement, shiftL, shiftR, (.&.),
                                           (.|.))
import           Data.Foldable       as X (Foldable)
import           Data.Int            as X
import           Data.IntCast        as X
import           Data.Traversable    as X (Traversable)
import           Data.Typeable       as X (Typeable)
import           Data.Word           as X
import           GHC.Generics        as X (Generic)
