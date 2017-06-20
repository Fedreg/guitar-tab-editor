module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import List.Extra as LE exposing (init)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { processedNotes : List TabEntry
    , enteredNotes : List String
    , tabXpos : Int
    , tabYpos : Int
    }


type alias TabEntry =
    { fret : Int
    , string : Int
    }


init =
    { processedNotes = []
    , enteredNotes = []
    , tabXpos = 0
    , tabYpos = 0
    }
        ! []


type Msg
    = ClearTab
    | NewStaff
    | ReadTab String


update msg model =
    case msg of
        NewStaff ->
            { model | tabXpos = model.tabXpos + 200 } ! []

        ReadTab notes ->
            let
                readNotes =
                    String.split " " notes

                parsedNotes =
                    parseInput readNotes

                yPos =
                    model.tabYpos + 20
            in
                { model | enteredNotes = readNotes, processedNotes = parsedNotes, tabYpos = yPos }
                    ! []

        ClearTab ->
            { model | processedNotes = [] } ! []


view model =
    div [ style [ ( "color", "#fff" ) ] ]
        [ tabInput
        , button [ onClick ClearTab ] [ text "Delete All" ]
        , tabLines
        , tabNotes model.processedNotes
        , div [ style [ ( "marginTop", "100px" ) ] ] [ text <| toString model.enteredNotes ]
        ]


tabNotes tabList =
    let
        tabHtmlList =
            []

        tabItem a =
            if a.fret == 50 then
                div [ style [ ( "color", "rgba(0,0,0,0)" ) ] ] [ text "-" ]
            else if a.string == 9 then
                div [ style [ ( "height", "85px" ), ( "border", "1px solid #333" ), ( "zIndex", "1" ), ( "margin", "5px 5px 0 15px" ) ] ] []
            else
                div [ style [ ( "position", "relative" ), ( "marginTop", (noteXpos a.string) ), ( "marginLeft", "10px" ) ] ] [ text <| toString a.fret ]
    in
        div [ style [ ( "display", "flex" ), ( "marginTop", "-100px" ) ] ]
            (List.map tabItem tabList)


tabLines =
    div [ style [ ( "marginTop", "50px" ), ( "position", "relative" ) ] ]
        [ hr [ style [ ( "border", "1px solid #333" ), ( "marginTop", "15px" ) ] ] []
        , hr [ style [ ( "border", "1px solid #333" ), ( "marginTop", "15px" ) ] ] []
        , hr [ style [ ( "border", "1px solid #333" ), ( "marginTop", "15px" ) ] ] []
        , hr [ style [ ( "border", "1px solid #333" ), ( "marginTop", "15px" ) ] ] []
        , hr [ style [ ( "border", "1px solid #333" ), ( "marginTop", "15px" ) ] ] []
        , hr [ style [ ( "border", "1px solid #333" ), ( "marginTop", "15px" ) ] ] []
        ]


tabInput =
    input
        [ type_ "text"
        , placeholder "Enter Tab"
        , onInput ReadTab
        , style
            [ ( "display", "inline" )
            , ( "width", "40%" )
            , ( "textAlign", "center" )
            , ( "margin", "100px 0 0 25%" )
            , ( "backgroundColor", "none" )
            ]
        ]
        []


parseInput noteList =
    List.map splitNotes noteList


splitNotes note =
    let
        fretNo =
            String.dropLeft 1 note
                |> String.toInt
                |> Result.withDefault 50

        stringNo =
            String.left 1 note
                |> String.toInt
                |> Result.withDefault 0
    in
        TabEntry fretNo stringNo


noteXpos a =
    case a of
        1 ->
            "-2px"

        2 ->
            "15px"

        3 ->
            "32px"

        4 ->
            "49px"

        5 ->
            "66px"

        6 ->
            "83px"

        _ ->
            "0"


subscriptions model =
    Sub.none
