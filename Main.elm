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
    div [ style [ ( "color", "#333" ), ( "textAlign", "center" ) ] ]
        [ tabInput
        , clearButton
        , tabLines
        , tabNotes model.processedNotes
        , instructions
        ]


{-| Takes in the list of notes that have been read in and processed and displays them as HTML.
-}
tabNotes : List (List Tab) -> Html Msg
tabNotes tabList =
    let
        tabItem a =
            -- Single note
            if List.length a == 1 then
                let
                    note =
                        List.head a
                            |> Maybe.withDefault { fret = 0, string = 1 }
                in
                    if note.fret == 50 then
                        div [ style [ ( "color", "rgba(0,0,0,0)" ) ] ] [ text "-" ]
                    else if note.string == 9 then
                        -- single line / bar end
                        div [ style [ ( "height", "85px" ), ( "border", "1px solid #aaa" ), ( "zIndex", "1" ), ( "margin", "5px 5px 0 15px" ) ] ] []
                    else if note.string == 8 then
                        -- double line
                        div [ style [ ( "height", "85px" ), ( "width", "3px" ), ( "borderLeft", "2px solid #aaa" ), ( "borderRight", "2px solid #aaa" ), ( "zIndex", "1" ), ( "margin", "5px 5px 0 15px" ) ] ] []
                    else if note.fret == 51 then
                        -- slur
                        div [ style [ ( "transform", "rotate(-90deg) translateX(10px) translateY(6px)" ), ( "marginTop", (noteXpos note.string) ) ] ] [ text ")" ]
                    else if note.fret == 52 then
                        -- slide up
                        div [ style [ ( "transform", "skewX(-45deg)" ), ( "margin", (noteXpos note.string) ++ " 0 0 10px" ) ] ] [ text "|" ]
                    else if note.fret == 53 then
                        -- slide down
                        div [ style [ ( "transform", "skewX(45deg)" ), ( "margin", (noteXpos note.string) ++ " 0 0 10px" ) ] ] [ text "|" ]
                    else
                        div [ style [ ( "position", "relative" ), ( "marginTop", (noteXpos note.string) ), ( "marginLeft", "10px" ) ] ] [ text <| toString note.fret ]
                -- Chord
            else
                let
                    mapper b =
                        if b.fret == 50 then
                            div [] []
                        else
                            div [ style [ ( "margin", "-1px 0 0 10px" ) ] ] [ text <| toString b.fret ]

                    finalDiv =
                        List.map mapper a
                in
                    div [ style [ ( "position", "relative" ) ] ] (List.reverse finalDiv)
    in
        div [ style [ ( "display", "flex" ), ( "marginTop", "-100px" ) ] ]
            (List.map tabItem tabList)


tabLines : Html Msg
tabLines =
    let
        lineStyle =
            style [ ( "border", "1px solid #aaa" ), ( "marginTop", "15px" ) ]
    in
        div [ style [ ( "marginTop", "50px" ), ( "position", "relative" ) ] ]
            [ hr [ lineStyle ] []
            , hr [ lineStyle ] []
            , hr [ lineStyle ] []
            , hr [ lineStyle ] []
            , hr [ lineStyle ] []
            , hr [ lineStyle ] []
            ]


tabInput : Html Msg
tabInput =
    textarea
        [ placeholder "Enter Tab"
        , onInput ReadTab
        , style
            [ ( "width", "60%" )
            , ( "height", "100px" )
            , ( "textAlign", "center" )
            , ( "margin", "100px 20% 0" )
            , ( "backgroundColor", "#fff" )
            , ( "color", "#03a9f4" )
            , ( "border", "1px solid #aaa" )
            , ( "fontSize", "16px" )
            ]
        ]
        []


clearButton : Html Msg
clearButton =
    button [ style [ ( "backgroundColor", "#ccc" ), ( "color", "#03a9f4" ), ( "border", "1px solid #aaa" ), ( "marginTop", "10px" ), ( "padding", "10px" ) ] ] [ text "Clear Tab" ]


{-| Maps input over splitNotes to format
-}
parseInput : List String -> List (List Tab)
parseInput noteList =
    List.map splitNotes noteList


{-| Determines if note is a fret/string combo, a chord, or a special symbol and formats accordingly
-}
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
    else if String.length note == 1 || String.slice 1 2 note == "#" then
        let
            mapper a =
                Tab (fretNo a) (stringNo a)
        in
            List.map mapper (chordTransform note)
    else
        [ Tab (fretNo note) (stringNo note) ]


{-| Takes in a 2 digit fret/string no and splits out just the fret
-}
fretNo : String -> Int
fretNo a =
    String.dropLeft 1 a
        |> String.toInt
        |> Result.withDefault 50


{-| Takes in a 2 digit fret/string no and splits out just the string
-}
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


{-| Turns named chords (i.e. "G", "C", into Tab )
-}
chordTransform : String -> List String
chordTransform note =
    case note of
        "A" ->
            [ "xx", "50", "42", "32", "22", "10" ]

        "a" ->
            [ "xx", "50", "42", "32", "21", "10" ]

        "B7" ->
            [ "xx", "52", "41", "32", "20", "12" ]

        "b" ->
            [ "xx", "xx", "44", "34", "23", "12" ]

        "C" ->
            [ "xx", "53", "42", "30", "21", "10" ]

        "D" ->
            [ "xx", "xx", "40", "32", "23", "12" ]

        "d" ->
            [ "xx", "xx", "40", "32", "23", "11" ]

        "e" ->
            [ "60", "52", "42", "30", "20", "10" ]

        "E" ->
            [ "60", "52", "42", "31", "20", "10" ]

        "F" ->
            [ "xx", "xx", "43", "32", "21", "10" ]

        "f#" ->
            [ "62", "50", "40", "32", "22", "10" ]

        "G" ->
            [ "63", "52", "40", "30", "23", "13" ]

        _ ->
            []


instructions : Html msg
instructions =
    ul [ style [ ( "position", "absolute" ), ( "bottom", "5%" ), ( "left", "25%" ), ( "width", "60%" ), ( "textAlign", "left" ), ( "color", "#333" ) ] ]
        [ li [] [ text "type string number directly followed by fret number, followed by a space, ex: 10 21 33 11, etc" ]
        , li [] [ text "type space as many times as desired to add spacing between notes." ]
        , li [] [ text "for chords type all 6 string number/fret number sets together. ex: G major = 635240302313" ]
        , li [] [ text "for chords with fewer than 6 strings use xx for string/fret. ex: D major = xxxx40322312" ]
        , li [] [ text "OR... You can just enter a chord name: G fom G major; a for a minor." ]
        , li [] [ text "type 99 for a barline." ]
        , li [] [ text "type 88 for a double barline." ]
        , li [] [ text "type STRING 51 for a slur mark; 52 for / 53 for \\  So if I want forward slide on string 3, 352" ]
        , li [] [ text "Experimental, more coming soon!" ]
        ]


subscriptions : a -> Sub msg
subscriptions model =
    Sub.none
