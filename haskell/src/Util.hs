module Util (
    map_many_on_one
) where


map_many_on_one :: [(a -> b)] -> a -> [b]
map_many_on_one [] _ = []
map_many_on_one (function: function_list) input =
    function input: map_many_on_one function_list input
