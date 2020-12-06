{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Debug.Trace (trace)
import Linear (V3 (..))

-- ct => "coordinate type"
newtype Point ct = Point
  { coordinates :: ct
  }
  deriving (Show, Eq)

makePoint :: a -> a -> a -> Point (Linear.V3 a)
makePoint x y z = Point (V3 x y z)

newtype instance UM.MVector s (Point ct) = MV_Point (UM.MVector s (Point ct))

newtype instance U.Vector (Point ct) = V_Point (U.Vector (Point ct))

instance (U.Unbox ct) => GM.MVector UM.MVector (Point ct) where
  {-# INLINE basicUnsafeNew #-}
  basicLength (MV_Point mv) = error "basicLength"
  basicUnsafeSlice i l (MV_Point mv) = error "basicUnsafeSlice"
  basicOverlaps (MV_Point mv) (MV_Point mv') = error "basicOverlaps"
  basicUnsafeNew l = trace "unsafe new" (MV_Point <$> GM.basicUnsafeNew l) -- <-- hangs!
  basicInitialize (MV_Point mv) = error "basicInitialize"
  basicUnsafeRead (MV_Point mv) i = error "basicUnsafeRead"
  basicUnsafeWrite (MV_Point mv) i x = error "basicUnsafeWrite"

instance (U.Unbox ct) => G.Vector U.Vector (Point ct) where
  basicUnsafeFreeze (MV_Point mv) = error "basicUnsafeFreeze"
  basicUnsafeThaw (V_Point v) = error "basicUnsafeThaw"
  basicLength (V_Point v) = error "basicLength"
  basicUnsafeSlice i l (V_Point v) = error "basicUnsafeSlice"
  basicUnsafeIndexM (V_Point v) i = error "basicUnsafeIndexM"

instance (U.Unbox ct) => U.Unbox (Point ct)

main :: IO ()
main = print $ U.length $ U.singleton $ makePoint (0.0 :: Double) 1.0 2.0
