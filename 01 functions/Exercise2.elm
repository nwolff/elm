module Main exposing (..)

import Html
import String


uppercaseLongNames : String -> String
uppercaseLongNames name =
    if String.length name > 10 then
        String.toUpper name
    else
        name


doIt : String -> String
doIt name =
    uppercaseLongNames name ++ " - name length: " ++ (toString (String.length name))


main =
    Html.text ((doIt "james moore") ++ (doIt "foo bar"))
