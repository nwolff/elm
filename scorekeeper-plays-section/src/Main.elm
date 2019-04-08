module Main exposing (Model, Msg(..), Play, Player, add, deletePlay, edit, init, main, playHeader, playList, playSection, playerForm, playerList, playerListHeader, playerSection, pointTotal, save, score, subscriptions, update, updateModel, view, viewPlay, viewPlayer)

import Browser exposing (Document)
import Html exposing (Html, button, div, footer, h1, header, i, input, li, text, ul)
import Html.Attributes exposing (class, placeholder, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import String



-- model


type alias Model =
    { players : List Player
    , name : String
    , playerId : Maybe Int
    , plays : List Play
    }


type alias Player =
    { id : Int
    , name : String
    , points : Int
    }


type alias Play =
    { id : Int
    , playerId : Int
    , name : String
    , points : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { players = []
      , name = ""
      , playerId = Nothing
      , plays = []
      }
    , Cmd.none
    )



-- update


type Msg
    = Edit Player
    | Score Player Int
    | Input String
    | Save
    | Cancel
    | DeletePlay Play


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updateModel msg model
    , Cmd.none
    )


updateModel : Msg -> Model -> Model
updateModel msg model =
    case msg of
        Input name ->
            { model | name = name }

        Cancel ->
            { model | name = "", playerId = Nothing }

        Save ->
            if String.isEmpty model.name then
                model

            else
                save model

        Score player points ->
            score model player points

        Edit player ->
            { model | name = player.name, playerId = Just player.id }

        DeletePlay play ->
            deletePlay model play


deletePlay : Model -> Play -> Model
deletePlay model playToDelete =
    let
        newPlays =
            List.filter
                (\play ->
                    play.id /= playToDelete.id
                )
                model.plays

        newPlayers =
            List.map
                (\player ->
                    if player.id == playToDelete.playerId then
                        { player | points = player.points - playToDelete.points }

                    else
                        player
                )
                model.players
    in
    { model | plays = newPlays, players = newPlayers }


score : Model -> Player -> Int -> Model
score model scorer points =
    let
        newPlayers =
            List.map
                (\player ->
                    if player.id == scorer.id then
                        { player
                            | points = player.points + points
                        }

                    else
                        player
                )
                model.players

        play =
            Play (List.length model.plays) scorer.id scorer.name points
    in
    { model | players = newPlayers, plays = play :: model.plays }


save : Model -> Model
save model =
    case model.playerId of
        Just id ->
            edit model id

        Nothing ->
            add model


edit : Model -> Int -> Model
edit model id =
    let
        newPlayers =
            List.map
                (\player ->
                    if player.id == id then
                        { player | name = model.name }

                    else
                        player
                )
                model.players

        newPlays =
            List.map
                (\play ->
                    if play.playerId == id then
                        { play | name = model.name }

                    else
                        play
                )
                model.plays
    in
    { model
        | players = newPlayers
        , plays = newPlays
        , name = ""
        , playerId = Nothing
    }


add : Model -> Model
add model =
    let
        player =
            Player (List.length model.players) model.name 0

        newPlayers =
            player :: model.players
    in
    { model
        | players = newPlayers
        , name = ""
    }



-- view


view : Model -> Document Msg
view model =
    Document
        "Scorekeeper"
        [ div [ class "scoreboard" ]
            [ h1 [] [ text "Score Keeper" ]
            , playerSection model
            , playerForm model
            , playSection model
            ]
        ]


playerSection : Model -> Html Msg
playerSection model =
    div []
        [ playerListHeader
        , playerList model
        , pointTotal model
        ]


playerListHeader : Html Msg
playerListHeader =
    header []
        [ div [] [ text "Name" ]
        , div [] [ text "Points" ]
        ]


playerList : Model -> Html Msg
playerList model =
    model.players
        |> List.sortBy .name
        |> List.map (viewPlayer model)
        |> ul []


viewPlayer : Model -> Player -> Html Msg
viewPlayer model player =
    let
        editPlayerClass =
            case model.playerId of
                Just id ->
                    if player.id == id then
                        "edit"

                    else
                        ""

                Nothing ->
                    ""
    in
    li []
        [ i
            [ class "edit"
            , onClick (Edit player)
            ]
            []
        , div [ class editPlayerClass ]
            [ text player.name ]
        , button
            [ type_ "button"
            , onClick (Score player 2)
            ]
            [ text "2pt" ]
        , button
            [ type_ "button"
            , onClick (Score player 3)
            ]
            [ text "3pt" ]
        , div []
            [ text (String.fromInt player.points) ]
        ]


pointTotal : Model -> Html Msg
pointTotal model =
    let
        total =
            List.map .points model.plays
                |> List.sum
    in
    footer []
        [ div [] [ text "Total:" ]
        , div [] [ text (String.fromInt total) ]
        ]


playerForm : Model -> Html Msg
playerForm model =
    let
        editInputClass =
            case model.playerId of
                Just _ ->
                    "edit"

                Nothing ->
                    ""
    in
    Html.form [ onSubmit Save ]
        [ input
            [ type_ "text"
            , placeholder "Add/Edit Player..."
            , onInput Input
            , value model.name
            , class editInputClass
            ]
            []
        , button [ type_ "submit" ] [ text "Save" ]
        , button [ type_ "button", onClick Cancel ] [ text "Cancel" ]
        ]


playSection : Model -> Html Msg
playSection model =
    div []
        [ playHeader
        , playList model
        ]


playHeader : Html Msg
playHeader =
    header []
        [ div [] [ text "Plays" ]
        , div [] [ text "Points" ]
        ]


playList : Model -> Html Msg
playList model =
    model.plays
        |> List.map viewPlay
        |> ul []


viewPlay : Play -> Html Msg
viewPlay play =
    li []
        [ i [ class "remove", onClick (DeletePlay play) ] []
        , div [] [ text play.name ]
        , div [] [ text (String.fromInt play.points) ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
