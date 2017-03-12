module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String
import Debug


-- model


type alias Model =
    { total : Int
    , input : String
    , error : Maybe String
    }


initModel : Model
initModel =
    { total = 0
    , input = ""
    , error = Nothing
    }



-- update


type Msg
    = AddCalories
    | Input String
    | Clear


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddCalories ->
            case String.toInt model.input of
                Ok c ->
                    { model | total = model.total + c, error = Nothing, input = "" }

                Err err ->
                    { model | error = Just err, input = "" }

        Input newInput ->
            { model | input = newInput }

        Clear ->
            initModel



-- view


view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text ("Total Calories: " ++ (toString model.total)) ]
        , input
            [ type_ "text"
            , onInput Input
            , value model.input
            ]
            []
        , div [] [ text (Maybe.withDefault "" model.error) ]
        , button
            [ type_ "button"
            , onClick AddCalories
            ]
            [ text "Add" ]
        , button
            [ type_ "button"
            , onClick Clear
            ]
            [ text "Clear" ]
        ]


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initModel
        , update = update
        , view = view
        }
