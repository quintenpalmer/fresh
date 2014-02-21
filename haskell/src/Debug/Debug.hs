module Debug.Debug (
    DebugShow,
    debug_show,
) where

class DebugShow a where
    debug_show :: a -> String
