import Data.Typeable hiding (gcast)
import Data.Data hiding (gcast)
import Data.Maybe (fromJust)
import Unsafe.Coerce

gcast :: (Typeable a, Typeable b) => c a -> Maybe (c b)
gcast ca = mcr
  where mcr = if typeOf (unc ca) == typeOf (unc $ fromJust mcr)
              then Just $ unsafeCoerce ca
              else Nothing
        unc :: c x -> x
        unc = undefined
