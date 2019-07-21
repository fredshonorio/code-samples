module Main exposing (main)

import Array
import Browser
import Color exposing (Color)
import Six exposing (Six)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time


type alias Model =
    { start : Maybe Time.Posix
    , uptimeMs : Float
    , orbits : Six Orbit
    }


type Msg
    = Tick Time.Posix


type alias Coord =
    { x : Float, y : Float }


type alias Orbit =
    { center : Coord
    , radius : Float
    , turnsSec : Float
    , startTurn : Float
    }


orbitRadius =
    50


centersAround : Coord -> Float -> Six Coord
centersAround center dist =
    let
        coord =
            \t -> Coord (center.x + turnX t dist) (center.y + turnY t dist)
    in
    Six.from (\n -> coord (toFloat n / 6))


orbitCenters =
    centersAround (Coord 400 400) 290


init _ =
    ( Model
        Nothing
        0
        (Six
            (Orbit orbitCenters.e0 orbitRadius -1 0)
            (Orbit orbitCenters.e1 orbitRadius -2 0)
            (Orbit orbitCenters.e2 orbitRadius -0.5 0)
            (Orbit orbitCenters.e3 orbitRadius -1 0)
            (Orbit orbitCenters.e4 orbitRadius -0.1 0)
            (Orbit orbitCenters.e5 orbitRadius -0.5 0)
        )
    , Cmd.none
    )


turnX ts radius =
    radius * cos (turns ts)


turnY turn radius =
    radius * sin (turns turn)


orbitPosition : Float -> Orbit -> Coord
orbitPosition timeMs { center, radius, turnsSec, startTurn } =
    let
        turns =
            startTurn + (turnsSec * (timeMs / 1000))
    in
    Coord (center.x + turnX turns radius) (center.y + turnY turns radius)


showMoon { x, y } =
    circle
        [ cx (String.fromFloat x)
        , cy (String.fromFloat y)
        , r "5"
        , fill "white"
        ]
        []


midpoint : Coord -> Coord -> Coord
midpoint from to =
    Coord ((from.x + to.x) / 2) ((from.y + to.y) / 2)


showSide : Color -> Coord -> Coord -> Svg a
showSide color from to =
    line
        [ x1 (String.fromFloat from.x)
        , y1 (String.fromFloat from.y)
        , x2 (String.fromFloat to.x)
        , y2 (String.fromFloat to.y)
        , stroke (Color.toCssString color)
        , strokeWidth "3"
        ]
        []


colors =
    Array.fromList [ Color.red, Color.orange, Color.yellow, Color.green, Color.blue, Color.purple ]


nColor : Int -> Color
nColor i =
    let
        n =
            remainderBy (Array.length colors) i
    in
    Array.get n colors |> Maybe.withDefault Color.black


showSides : Int -> Six Coord -> List (Svg a)
showSides color coords =
    coords |> Six.pairedOffset1 (showSide (nColor color)) |> Six.toList


showOrbit coord =
    circle
        [ cx (String.fromFloat coord.x)
        , cy (String.fromFloat coord.y)
        , r (String.fromFloat orbitRadius)
        , fill "none"
        , stroke "grey"
        , strokeWidth "2"
        ]
        []


background =
    rect [ width "100%", height "100%", fill "black" ] []


shownOrbits =
    orbitCenters |> Six.toList |> List.map showOrbit


view model =
    let
        moons =
            model.orbits |> Six.map (orbitPosition model.uptimeMs)

        shownMidpoints =
            moons
                |> midpointLevels 15
                |> List.indexedMap showSides
                |> List.concatMap identity

        shownMoons =
            moons |> Six.toList |> List.map showMoon
    in
    svg
        [ width "800"
        , height "800"
        , viewBox "0 0 800 800"
        ]
        (background :: shownOrbits ++ shownMidpoints ++ shownMoons)



-- generate n levels of midpoints


midpointLevels : Int -> Six Coord -> List (Six Coord)
midpointLevels n coords =
    if n >= 1 then
        coords :: midpointLevels (n - 1) (Six.pairedOffset1 midpoint coords)

    else
        []


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick posix ->
            case model.start of
                Just start ->
                    -- update uptime
                    { model | uptimeMs = toFloat (Time.posixToMillis posix - Time.posixToMillis start) }

                Nothing ->
                    -- set start time the first time
                    { model | start = Just posix }



-- TODO: use a correct loop


subs _ =
    Time.every 16 Tick


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subs
        }
