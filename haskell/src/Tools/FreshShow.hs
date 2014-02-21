module Tools.FreshShow (
    FullShow,
    full_show,
    ShortShow,
    short_show
) where

class FullShow a where
    full_show :: a -> String

class ShortShow a where
    short_show :: a -> String
