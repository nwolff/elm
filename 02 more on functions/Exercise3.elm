module Main exposing (..)

import Html
import String


wordCount : String -> Int
wordCount =
    String.words >> List.length


main =
    wordCount "the quick brown fox"
        |> toString
        |> Html.text
