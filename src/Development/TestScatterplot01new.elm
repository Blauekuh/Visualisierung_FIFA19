module Development.TestScatterplot01new exposing (..)

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
    | Loading
    | Success
        { data : List Footballer
        , xAAFunction : (Footballer -> Float)
        , yAAFunction : (Footballer -> Float)
        , xName : String
        , yName : String
        }
    


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading 
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
    [ "dataHeight.csv" ]


csvString_to_data : String -> List Footballer
csvString_to_data csvRaw =
    Csv.parse csvRaw
        |> Csv.Decode.decodeCsv decodeFootballer
        |> Result.toMaybe
        |> Maybe.withDefault []


type alias Footballer =
    { name : String, age : Float, overall : Float, potential : Float}--, height : Float }


decodeFootballer : Csv.Decode.Decoder ( Footballer -> a) a
decodeFootballer =
    Csv.Decode.map Footballer 
        (Csv.Decode.field "Name" Ok 
            |> Csv.Decode.andMap (Csv.Decode.field "Age" (String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "Overall" (String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "Potential" (String.toFloat >> Result.fromMaybe "error parsing string"))
            --|> Csv.Decode.andMap (Csv.Decode.field "Height" (String.toFloat >> Result.fromMaybe "error parsing string"))
        )



-- UPDATE


type Msg
    = GotText (Result Http.Error String)
    | ChangeX (Footballer -> Float, String)
    | ChangeY (Footballer -> Float, String)



--| UpdateAttribute1 Footballer


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    ( Success <| { data = spielerListe [ fullText ], xAAFunction = .age, yAAFunction = .overall , xName = "Age", yName = "Overall"}, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


        ChangeX (x, a) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, xAAFunction = x, yAAFunction = m.yAAFunction, xName = a, yName = m.yName }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ChangeY (y, a) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, xAAFunction = m.xAAFunction, yAAFunction = y, xName = m.xName, yName = a }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


spielerListe : List String -> List Footballer
spielerListe liste1 =
    List.map (\t -> csvString_to_data t) liste1
        |> List.concat
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
    svg [ viewBox 0 0 w h, TypedSvg.Attributes.width <| TypedSvg.Types.Percent 80, TypedSvg.Attributes.height <| TypedSvg.Types.Percent 80 ]
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
        [ circle [ cx 0, cy 0, r 3 ] []
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
    XyData x y (List.map (\n -> pointName n a b c x y) playerlist)




type alias Point =
    { pointName : String, x : Float, y : Float }


pointName : Footballer -> (Footballer -> String) -> (Footballer -> Float) -> (Footballer -> Float) -> String -> String -> Point
pointName player a b c d e =
    Point (a player ++ ", " ++ d ++ ": " ++ String.fromFloat (b player) ++ "," ++ e ++ ": " ++ String.fromFloat (c player)) (b player) (c player)

--umwandeln : List (Maybe Footballer) -> List Footballer
--umwandeln players =
--    List.map (\n -> n.age  Maybe.withDefault 0.0 ) players
--    |>List.map (\n -> n.overall  Maybe.withDefault 0.0 ) players
--    |>List.map (\n -> n.height  Maybe.withDefault 0.0 ) players
--    |>List.map (\n -> n.potential  Maybe.withDefault 0.0 ) players

-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Failure ->
            Html.text "I was unable to load the players."

        Loading  ->
            Html.text "Loading Players..."


        Success l ->
            let 
                
                spieler =
                    filterAndReducePlayers l.data .name l.xAAFunction l.yAAFunction l.xName l.yName
            in
            Html.div []
                [
                 ul []
                    [ li [] [
                            Html.text <| "Set X Value"
                            , Html.button [ onClick (ChangeX (.overall, "Overall")) ] [ Html.text "Overall" ]
                            , Html.button [ onClick (ChangeX (.potential, "Potential")) ] [ Html.text "Potential" ]
                            , Html.button [ onClick (ChangeX (.age, "Age")) ] [ Html.text "Age" ]
                            --, Html.button [ onClick (ChangeX (.height, "Height")) ] [ Html.text "Height" ]
                            ]
                    ]
                , ul []
                    [ li [] [
                            Html.text <| "Set Y Value"
                            , Html.button [ onClick (ChangeY (.overall, "Overall")) ] [ Html.text "Overall" ]
                            , Html.button [ onClick (ChangeY (.potential, "Potential")) ] [ Html.text "Potential" ]
                            , Html.button [ onClick (ChangeY (.age, "Age")) ] [ Html.text "Age" ]
                            --, Html.button [ onClick (ChangeY (.height, "Height")) ] [ Html.text "Height" ]
                            ]
                    ]            
                --  , Html.input [ Html.Attributes.placeholder "Age", Html.Attributes.value model.content, onInput Change ]
                , scatterplot spieler
                ]
