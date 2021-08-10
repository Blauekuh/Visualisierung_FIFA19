module TestScatterplot01new exposing (..)

import Axis
import Browser
import Csv
import Csv.Decode
import Html exposing (Html, a, li, pre, text, ul)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)
import Http
import Scale exposing (ContinuousScale)
import Statistics
import TypedSvg exposing (circle, g, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, fontFamily, fontSize, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), FontWeight(..), Length(..), Transform(..), px)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Model
    = Failure
    | Loading String String
    | Success
        { data : List Footballer
        , xAAFunction : Footballer -> Float
        , yAAFunction : Footballer -> Float
        }
    | Success2 (List Footballer)



--| Success3 (List String) String String


type Attribute1
    = Attribute1 String


type Attribute2
    = Attribute2 String


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading "" ""
    , getRandomCatGif GotText
    )


getRandomCatGif : (Result Http.Error String -> Msg) -> Cmd Msg
getRandomCatGif x =
    liste
        |> List.map
            (\datensatz ->
                Http.get
                    { url = "https://raw.githubusercontent.com/JohannesLange/Visualisierung_FIFA19/master/" ++ datensatz
                    , expect = Http.expectString x
                    }
            )
        |> Cmd.batch


liste : List String
liste =
    [ "data.csv" ]


csvString_to_data : String -> List Footballer
csvString_to_data csvRaw =
    Csv.parse csvRaw
        |> Csv.Decode.decodeCsv decodeFootballer
        |> Result.toMaybe
        |> Maybe.withDefault []


type alias Footballer =
    { name : String, age : Float, overall : Float, potential : Float }


decodeFootballer : Csv.Decode.Decoder (Footballer -> a) a
decodeFootballer =
    Csv.Decode.map Footballer
        (Csv.Decode.field "Name" Ok 
            |> Csv.Decode.andMap (Csv.Decode.field "Age" (String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "Overall" (String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "Potential" (String.toFloat >> Result.fromMaybe "error parsing string"))
        )



-- UPDATE


type Msg
    = GotText (Result Http.Error String)
    | GotText2 (Result Http.Error String)
    | MorePlease
    | MorePlease2
    | Change ( Footballer -> Float, Footballer -> Float )
    | ChangeX (Footballer -> Float)
    | ChangeY (Footballer -> Float)



--| UpdateAttribute1 Footballer


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    ( Success <| { data = spielerListe [ fullText ], xAAFunction = .age, yAAFunction = .overall }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        GotText2 result ->
            case result of
                Ok fullText ->
                    ( Success2 <| spielerListe [ fullText ], Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        MorePlease ->
            ( Loading "" "", getRandomCatGif GotText2 )

        MorePlease2 ->
            ( Loading "" "", getRandomCatGif GotText )

        Change ( x, y ) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, xAAFunction = x, yAAFunction = y }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ChangeX x ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, xAAFunction = x, yAAFunction = m.yAAFunction }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ChangeY y ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, xAAFunction = m.xAAFunction, yAAFunction = y }, Cmd.none )

                _ ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    60


radius : Float
radius =
    5.0


tickCount : Int
tickCount =
    5


defaultExtent : ( number, number1 )
defaultExtent =
    ( 0, 100 )


scatterplot : XyData -> Svg msg
scatterplot model =
    let
        xValues : List Float
        xValues =
            List.map .x model.data

        yValues : List Float
        yValues =
            --[0, 200000]
            List.map .y model.data

        xScaleLocal : ContinuousScale Float
        xScaleLocal =
            xScale xValues

        yScaleLocal : ContinuousScale Float
        yScaleLocal =
            yScale yValues

        half : ( Float, Float ) -> Float
        half t =
            (Tuple.second t - Tuple.first t) / 2

        labelPositions : { x : Float, y : Float }
        labelPositions =
            { x = wideExtent xValues |> half
            , y = wideExtent yValues |> Tuple.second
            }
    in
    svg [ viewBox 0 0 w h, TypedSvg.Attributes.width <| TypedSvg.Types.Percent 100, TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100 ]
        [ style [] [ TypedSvg.Core.text """
            .point circle { stroke: rgba(0, 0, 0,0.4); fill: rgba(255, 255, 255,0.3); }
            .point text { display: none; }
            .point:hover circle { stroke: rgba(0, 0, 0,1.0); fill: rgb(118, 214, 78); }
            .point:hover text { display: inline; }
          """ ]
        , g [ transform [ Translate 60 390 ] ]
            [ xAxis xValues
            , text_
                [ x 360 --(Scale.convert xScaleLocal labelPositions.x)
                , y 35

                -- , fontFamily [ "Helvetica", "sans-serif" ]
                , fontSize (px 20)

                --, fontWeight FontWeightBold
                ]
                [ TypedSvg.Core.text model.xDescription ]
            ]
        , g [ transform [ Translate 60 60 ] ]
            [ yAxis yValues
            , text_
                [ x -30
                , y -30

                -- , fontFamily [ "Helvetica", "sans-serif" ]
                , fontSize (px 20)

                --, fontWeight FontWeightBold
                ]
                [ TypedSvg.Core.text model.yDescription ]
            ]
        , g [ transform [ Translate padding padding ] ]
            (List.map (point xScaleLocal yScaleLocal) model.data)
        ]


point : ContinuousScale Float -> ContinuousScale Float -> Point -> Svg msg
point scaleX scaleY xyPoint =
    g
        [ class [ "point" ]
        , fontSize <| Px 15.0
        , fontFamily [ "serif" ]
        , transform
            [ Translate
                (Scale.convert scaleX xyPoint.x)
                (Scale.convert scaleY xyPoint.y)
            ]
        ]
        [ circle [ cx 0, cy 0, r 5 ] []
        , text_ [ x 10, y -20, textAnchor AnchorMiddle ] [ Html.text xyPoint.pointName ]
        ]


type alias XyData =
    { xDescription : String
    , yDescription : String
    , data : List Point
    }


xScale : List Float -> ContinuousScale Float
xScale values =
    Scale.linear ( 0, w - 2 * padding ) (wideExtent values)


yScale : List Float -> ContinuousScale Float
yScale values =
    Scale.linear ( h - 2 * padding, 0 ) (wideExtent values)


addieren : ( Float, Float ) -> Float -> ( Float, Float )
addieren ( min, max ) shift =
    if min <= 0 then
        ( 0, max + shift )

    else
        ( min - shift, max + shift )


wideExtent : List Float -> ( Float, Float )
wideExtent values =
    let
        result =
            Maybe.withDefault ( 0, 0 )
                (Statistics.extent values)

        max =
            Maybe.withDefault 0
                (List.maximum values)

        result1 =
            addieren result (toFloat tickCount * max / 50)

        result2 =
            addieren result1 0.0
    in
    result2


xAxis : List Float -> Svg msg
xAxis values =
    Axis.bottom [ Axis.tickCount tickCount ] (xScale values)


yAxis : List Float -> Svg msg
yAxis values =
    Axis.left [ Axis.tickCount tickCount ] (yScale values)


filterAndReducePlayers : List Footballer -> (Footballer -> String) -> (Footballer -> Float) -> (Footballer -> Float) -> String -> String -> XyData
filterAndReducePlayers playerlist a b c x y =
    XyData x y (List.map (\n -> pointName n a b c) playerlist)



--filterAndReducePlayers : List Footballer -> String -> String -> XyData
--filterAndReducePlayers playerlist x y =
--    if x  == "Age" then
--        if y == "Overall" then
--                XyData x y (List.map (\n -> pointName x y n.name n.age n.overall) playerlist)
--        else --y == "Potential" then
--                XyData x y (List.map (\n -> pointName x y n.name n.age n.potential) playerlist)
--    else if x == "Overall" then
--        if y == "Potential" then
--                XyData x y (List.map (\n -> pointName x y n.name n.overall n.potential) playerlist)
--        else    XyData x y  (List.map (\n -> pointName x y n.name n.overall n.potential) playerlist)
--   else  XyData x y (List.map (\n -> pointName x y n.name n.age n.age) playerlist)


type alias Point =
    { pointName : String, x : Float, y : Float }


pointName : Footballer -> (Footballer -> String) -> (Footballer -> Float) -> (Footballer -> Float) -> Point
pointName player a b c =
    Point (a player ++ ", " ++ a player ++ ": " ++ String.fromFloat (b player) ++ "," ++ a player ++ ": " ++ String.fromFloat (c player)) (b player) (c player)



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Failure ->
            Html.text "I was unable to load the players."

        Loading "" "" ->
            Html.text "Loading Players..."

        Loading _ _ ->
            Html.text "Loading new Players ..."

        Success l ->
            let
                spieler =
                    filterAndReducePlayers l.data .name l.xAAFunction l.yAAFunction "Age" "Overall"
            in
            Html.div []
                [ Html.button [ onClick (Change ( .overall, .potential )) ] [ Html.text "Overall & Potential" ]
                , Html.button [ onClick (ChangeX .potential) ] [ Html.text "Potential" ]

                --  , Html.input [ Html.Attributes.placeholder "Age", Html.Attributes.value model.content, onInput Change ]
                , scatterplot spieler
                ]

        Success2 l ->
            let
                spieler =
                    filterAndReducePlayers l .name .overall .potential "Overall" "Potential"
            in
            Html.div []
                [ Html.button [ onClick MorePlease2 ] [ Html.text "Age & Overall" ]
                , scatterplot spieler
                ]


spielerListe : List String -> List Footballer
spielerListe liste1 =
    List.map (\t -> csvString_to_data t) liste1
        |> List.concat
