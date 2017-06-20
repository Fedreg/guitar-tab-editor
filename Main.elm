module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { processedNotes : List (List Tab)
    , enteredNotes : List String
    , tabXpos : Int
    , tabYpos : Int
    }


init =
    { processedNotes = []
    , enteredNotes = []
    , tabXpos = 0
    , tabYpos = 0
    }
        ! []


type alias Tab =
    { fret : Int
    , string : Int
    }


type alias Chord =
    List Tab


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


view : Model -> Html Msg
view model =
    div [ style [ ( "color", "#fff" ) ] ]
        [ tabInput
        , button [ onClick ClearTab ] [ text "Delete All" ]
        , tabLines
        , tabNotes model.processedNotes
        ]


tabNotes : List (List Tab) -> Html Msg
tabNotes tabList =
    let
        tabItem a =
            if List.length a == 1 then
                let
                    note =
                        List.head a
                            |> Maybe.withDefault { fret = 0, string = 1 }
                in
                    if note.fret == 50 then
                        div [ style [ ( "color", "rgba(0,0,0,0)" ) ] ] [ text "-" ]
                    else if note.string == 9 then
                        div [ style [ ( "height", "85px" ), ( "border", "1px solid #333" ), ( "zIndex", "1" ), ( "margin", "5px 5px 0 15px" ) ] ] []
                    else
                        div [ style [ ( "position", "relative" ), ( "marginTop", (noteXpos note.string) ), ( "marginLeft", "10px" ) ] ] [ text <| toString note.fret ]
            else
                let
                    mapper b =
                        if b.fret == 50 then
                            div [] []
                        else
                            div [ style [ ( "position", "absolute" ), ( "top", (noteXpos b.string) ), ( "left", "10px" ) ] ] [ text <| toString b.fret ]

                    finalDiv =
                        List.map mapper a
                in
                    div [ style [ ( "position", "relative" ) ] ] finalDiv
    in
        div [ style [ ( "display", "flex" ), ( "marginTop", "-100px" ) ] ]
            (List.map tabItem tabList)


tabLines : Html Msg
tabLines =
    div [ style [ ( "marginTop", "50px" ), ( "position", "relative" ) ] ]
        [ hr [ style [ ( "border", "1px solid #333" ), ( "marginTop", "15px" ) ] ] []
        , hr [ style [ ( "border", "1px solid #333" ), ( "marginTop", "15px" ) ] ] []
        , hr [ style [ ( "border", "1px solid #333" ), ( "marginTop", "15px" ) ] ] []
        , hr [ style [ ( "border", "1px solid #333" ), ( "marginTop", "15px" ) ] ] []
        , hr [ style [ ( "border", "1px solid #333" ), ( "marginTop", "15px" ) ] ] []
        , hr [ style [ ( "border", "1px solid #333" ), ( "marginTop", "15px" ) ] ] []
        ]


tabInput : Html Msg
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


parseInput : List String -> List (List Tab)
parseInput noteList =
    List.map splitNotes noteList


splitNotes : String -> List Tab
splitNotes note =
    if String.length note == 12 then
        let
            e6 =
                String.slice 0 2 note

            a =
                String.slice 2 4 note

            d =
                String.slice 4 6 note

            g =
                String.slice 6 8 note

            b =
                String.slice 8 10 note

            e =
                String.slice 10 12 note

            mapper a =
                Tab (fretNo a) (stringNo a)
        in
            List.map mapper [ e6, a, d, g, b, e ]
    else
        [ Tab (fretNo note) (stringNo note) ]


fretNo : String -> Int
fretNo a =
    String.dropLeft 1 a
        |> String.toInt
        |> Result.withDefault 50


stringNo : String -> Int
stringNo a =
    String.left 1 a
        |> String.toInt
        |> Result.withDefault 0


noteXpos : Int -> String
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
