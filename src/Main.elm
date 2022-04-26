module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Svg
import Svg.Attributes

main : Program () Model Msg
main = Browser.sandbox { init = init, update = update, view = view }

type alias Clock =
    { description : String
    , wedges : Int
    , filled : Int
    }

type alias Model =
    { clocks : List Clock
    , newClockDesc : String
    , newClockWedges : String
    , newClockFilled : String
    , showNew : Bool
    }

init : Model
init = Model [] "" "" "" False

type Msg
    = AddClock String String String
    | UpdateClock String String String
    | IncrementClock String
    | DecrementClock String
    | RemoveClock String
    | ShowNew
    | CancelNew

update : Msg -> Model -> Model
update msg model =
    case msg of
        AddClock description wedges filled ->
            let intWedges = Maybe.withDefault 0 (String.toInt wedges)
                intFilled = Maybe.withDefault 0 (String.toInt filled)
            in { model | clocks = (Clock description intWedges intFilled)::model.clocks, newClockDesc = "", newClockWedges = "", newClockFilled = "", showNew = False }
        UpdateClock description wedges filled ->
            { model | newClockDesc = description, newClockWedges = wedges, newClockFilled = filled }
        IncrementClock description ->
            { model | clocks = increment description model.clocks }
        DecrementClock description ->
            { model | clocks = decrement description model.clocks }
        RemoveClock description ->
            { model | clocks = List.filter (\clock -> clock.description /= description) model.clocks }
        ShowNew ->
            { model | showNew = True }
        CancelNew ->
            { model | newClockDesc = "", newClockWedges = "", newClockFilled = "", showNew = False }

increment : String -> List Clock -> List Clock
increment desc clocks =
    clocks
    |> List.map (\clock -> if clock.description == desc then incremented clock else clock)

incremented : Clock -> Clock
incremented clock =
    { clock | filled = if clock.filled < clock.wedges then clock.filled + 1 else clock.filled }

decrement : String -> List Clock -> List Clock
decrement desc clocks =
    clocks
    |> List.map (\clock -> if clock.description == desc then decremented clock else clock)

decremented : Clock -> Clock
decremented clock =
    { clock | filled = if clock.filled > 0 then clock.filled - 1 else clock.filled }

view : Model -> Html Msg
view model =
    div []
    [ h1 [] [ text "Progress Clocks" ]
    , if model.showNew then viewNewEntry model.newClockDesc model.newClockWedges model.newClockFilled else viewNewButton
    , viewClocks model.clocks
    ]

viewNewEntry : String -> String -> String -> Html Msg
viewNewEntry currentDesc currentWedges currentFilled =
    div []
    [ h2 [] [ text "New Clock" ]
    , input [ placeholder "Description", value currentDesc, onInput (\s -> UpdateClock s currentWedges currentFilled) ] []
    , input [ placeholder "Total Wedges", value currentWedges, onInput (\s -> UpdateClock currentDesc s currentFilled), type_ "number" ] []
    , input [ placeholder "Currently Filled", value currentFilled, onInput (\s -> UpdateClock currentDesc currentWedges s), type_ "number" ] []
    , button [ onClick (AddClock currentDesc currentWedges currentFilled) ] [ text "Add" ]
    , button [ onClick CancelNew ] [ text "Cancel" ]
    ]

viewNewButton : Html Msg
viewNewButton =
    div []
    [ button [ onClick ShowNew ] [ text "New Clock" ]
    ]

viewClocks : List Clock -> Html Msg
viewClocks clocks =
    div []
    ([ h2 [] [ text "Clocks" ]
    ] ++ List.map viewClock clocks)

viewClock : Clock -> Html Msg
viewClock clock =
    div []
    [ h3 [] [ text clock.description ]
    , viewClockSvg clock.filled clock.wedges
    , button [ onClick (IncrementClock clock.description) ] [ text "Increment" ]
    , button [ onClick (DecrementClock clock.description) ] [ text "Decrement" ]
    , button [ onClick (RemoveClock clock.description) ] [ text "Delete" ]
    ]

viewClockSvg : Int -> Int -> Html Msg
viewClockSvg current total =
    let radius = 100.0
        diameter = String.fromFloat (radius * 2)
        viewBox = "0 0 " ++ diameter ++ " " ++ diameter
    in Svg.svg [ Svg.Attributes.width diameter, Svg.Attributes.height diameter, Svg.Attributes.viewBox viewBox ]
    (List.range 1 total
    |> List.map (\index ->
        let start = wedgeEnd (index - 1) total radius (radius, radius)
            end = wedgeEnd index total radius (radius, radius)
        in wedge start end (if index > current then "white" else "red"))
    )

wedgeEnd : Int -> Int -> Float -> (Float, Float) -> (Float, Float)
wedgeEnd index total radius (originX, originY) =
    let tau = pi * 2
        halfPi = pi / 2 -- used to rotate clock so that it "starts" at "12"
        i = toFloat index
        t = toFloat total
    in (
        originX + radius * cos (i * tau / t - halfPi),
        originY + radius * sin (i * tau / t - halfPi)
    )

wedge : (Float, Float) -> (Float, Float) -> String -> Svg.Svg msg
wedge (startX, startY) (endX, endY) fillColour =
    let start = (String.fromFloat startX) ++ "," ++ (String.fromFloat startY)
        end = (String.fromFloat endX) ++ "," ++ (String.fromFloat endY)
        arc = "M100,100 L" ++ start ++ " A100,100 0 0,1 " ++ end ++ " Z"
    in Svg.path [ Svg.Attributes.fill fillColour, Svg.Attributes.stroke "black", Svg.Attributes.d arc ] []
