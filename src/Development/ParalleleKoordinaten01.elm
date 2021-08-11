module Development.ParalleleKoordinaten01 exposing (..)


import Axis
import Browser
import Color
import Csv
import Csv.Decode
import Html exposing (Html, a, li, pre, text, ul)
import Html.Attributes exposing (placeholder, value, href)
import Html.Events exposing (onClick, onInput)
import Http
import List.Extra
import Path
import Scale exposing (ContinuousScale)
import Shape
import Statistics
import TypedSvg exposing (circle, g, line, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, d, fill, fontFamily, fontSize, stroke, strokeWidth, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, x1, y, y1, y2)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), FontWeight(..), Length(..), Paint(..), Transform(..), px)



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
        , xAAFunction : Footballer -> Float
        , yAAFunction : Footballer -> Float
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
    [ "data.csv" ]


csvString_to_data : String -> List Footballer
csvString_to_data csvRaw =
    Csv.parse csvRaw
        |> Csv.Decode.decodeCsv decodeFootballer
        |> Result.toMaybe
        |> Maybe.withDefault []


type alias Footballer =
    { name : String, age : Float, overall : Float, potential : Float, nationality: String }


decodeFootballer : Csv.Decode.Decoder (Footballer -> a) a
decodeFootballer =
    Csv.Decode.map Footballer
        (Csv.Decode.field "Name" Ok 
            |> Csv.Decode.andMap (Csv.Decode.field "Age" (String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "Overall" (String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "Potential" (String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "Nationality" Ok)
        )

type Msg
    = GotText (Result Http.Error String)
    --| MorePlease
    | Change (Footballer -> Float, Footballer -> Float)
    | ChangeX (Footballer -> Float, String)
    | ChangeY (Footballer -> Float, String)

    
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    ( Success <| { data = spielerListe [ fullText ], xAAFunction = .age, yAAFunction = .overall , xName = "Age", yName = "Overall"}, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


        --MorePlease ->
        --    ( Loading "" "", getRandomCatGif GotText2 )


        Change (x, y) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, xAAFunction = x, yAAFunction = y, xName = m.xName, yName = m.yName}, Cmd.none )

                _ ->
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

padding : Float
padding =
    60


radius : Float
radius =
    5.0


tickCount_ : Int
tickCount_ =
    8


defaultExtent : ( number, number1 )
defaultExtent =
    ( 0, 100 )


wideExtent : List Float -> ( Float, Float )
wideExtent values =
    let
        closeExtent =
            Statistics.extent values
                |> Maybe.withDefault defaultExtent

        extension =
            (Tuple.second closeExtent - Tuple.first closeExtent) / toFloat (2 * tickCount_)
    in
    ( Tuple.first closeExtent - extension |> max 0
    , Tuple.second closeExtent + extension
    )

type alias MultiDimPoint =
    { pointName : String, value : List Float }


type alias MultiDimData =
    { dimDescription : List String
    , data : List (List MultiDimPoint)
    }



parallelCoodinatesPlot : Float -> Float -> MultiDimData -> Svg msg
parallelCoodinatesPlot w ar model =
    let
        h : Float
        h =
            w / ar

        listTransformieren : List (List Float)
        listTransformieren =
            model.data
                |> List.concat
                |> List.map .value
                |> List.Extra.transpose

        listWideExtent : List ( Float, Float )
        listWideExtent =
            listTransformieren |> List.map wideExtent

        listScale =
            List.map (Scale.linear ( h, 0 )) listWideExtent

        listAxis =
            List.map (Axis.left [ Axis.tickCount tickCount_ ]) listScale

        xScale =
            Scale.linear ( 0, w ) ( 1, List.length model.dimDescription |> toFloat )
    in
    svg
        [ viewBox 0 0 (w + 2 * padding) (h + 2 * padding)
        , TypedSvg.Attributes.width <| TypedSvg.Types.Percent 90
        , TypedSvg.Attributes.height <| TypedSvg.Types.Percent 90
        ]
    <|
        [ TypedSvg.style []
            []
        , g [ TypedSvg.Attributes.class [ "parallelAxis" ] ]
            [ g [ transform [ Translate (padding - 1) padding ] ] <|
                List.indexedMap
                    (\i axis ->
                        g
                            [ transform
                                [ Translate (Scale.convert xScale (toFloat i + 1)) 0
                                ]
                            ]
                            [ axis ]
                    )
                    listAxis
            , g [ transform [ Translate (padding - 1) 0 ] ] <|
                List.indexedMap
                    (\i desc ->
                        text_
                            [ fontFamily [ "sans-serif" ]
                            , fontSize (Px 10)
                            , x <| Scale.convert xScale (toFloat i + 1)
                            , y <| padding * 7 / 8
                            , textAnchor AnchorMiddle
                            ]
                            [ TypedSvg.Core.text desc ]
                    )
                    model.dimDescription
            ]
        ]
            ++ (let
                    drawPoint p =
                        let
                            linePath : Path.Path
                            linePath =
                                List.map3
                                    (\desc s px ->
                                        Just
                                            ( Scale.convert xScale <| toFloat desc
                                            , Scale.convert s px
                                            )
                                    )
                                    (List.range 1 (List.length model.dimDescription))
                                    listScale
                                    p
                                    |> Shape.line Shape.linearCurve
                        in
                        Path.element linePath
                            [ stroke <| Paint <| Color.rgba 0 0 0 0.8
                            , strokeWidth <| Px 0.5
                            , fill PaintNone
                            ]
                in
                model.data
                    |> List.map
                        (\dataset ->
                            g [ transform [ Translate (padding - 1) padding ] ]
                                (List.map (.value >> drawPoint) dataset)
                        )
               )

view : Model -> Html Msg
view model =
    case model of
        Failure ->
            Html.text "I was unable to load the players."

        Loading  ->
            Html.text "Loading Players..."


        Success l ->
            let
                multiDimDaten : List Footballer -> (Footballer -> Float) -> (Footballer -> Float) -> (Footballer -> Float) -> (Footballer -> Float) -> (Footballer -> String) -> String -> String -> String -> String-> MultiDimData
                multiDimDaten listPlayers a b c d e f g h i=
                    MultiDimData [f, g, h, i]
                        [ List.map
                            (\x ->
                                [(a x), (b x), (c x), (d x)]
                                    |> MultiDimPoint (e x)
                            )
                            listPlayers
                        ]
            in
            Html.div []
                [ parallelCoodinatesPlot 600 2 (multiDimDaten l.data .overall .age .potential .age .name "Overall" "Age" "Potential" "Overall")
                ]