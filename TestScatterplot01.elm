

module Main exposing (..)

import Axis
import Browser
import Csv
import Csv.Decode
import Html exposing (Html, pre, text, a, li, ul)
import Html.Events exposing (onClick)
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
    | Success (List String)
    | Success2 (List String)




    




init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , getRandomCatGif
    )


getRandomCatGif : Cmd Msg
getRandomCatGif =
    liste
        |> List.map
            (\datensatz ->
                Http.get
                    { url = "https://raw.githubusercontent.com/JohannesLange/Visualisierung_FIFA19/master/" ++ datensatz
                    , expect = Http.expectString GotText
                    }
            )
        |> Cmd.batch


getRandomCatGif2 : Cmd Msg
getRandomCatGif2 =
    liste
        |> List.map
            (\datensatz ->
                Http.get
                    { url = "https://raw.githubusercontent.com/JohannesLange/Visualisierung_FIFA19/master/" ++ datensatz
                    , expect = Http.expectString GotText2
                    }
            )
        |> Cmd.batch
--|> Cmd.batch


liste : List String
liste =
    [ "data.csv" ]


csvString_to_data : String -> String -> String -> List ( String, Maybe Float, Maybe Float )
csvString_to_data csvRaw variable1 variable2 =
    Csv.parse csvRaw
        |> Csv.Decode.decodeCsv (decodeStockDay variable1 variable2)
        |> Result.toMaybe
        |> Maybe.withDefault []


decodeStockDay : String -> String -> Csv.Decode.Decoder (( String, Maybe Float, Maybe Float ) -> a) a
decodeStockDay variable1 variable2 =
    Csv.Decode.map (\a b c-> ( a, Just b, Just c ))
        (Csv.Decode.field "Name" Ok
            |> Csv.Decode.andMap
                (Csv.Decode.field variable1
                    (String.toFloat >> Result.fromMaybe "error parsing string")
                    |> Csv.Decode.andMap
                        (Csv.Decode.field variable2 
                            (String.toFloat >> Result.fromMaybe "error parsing string")
                                
                )
        )
        )

umwandeln : List ( String, Maybe Float, Maybe Float ) -> List ( String, String, String )
umwandeln ganzerText =
    List.map (\( a, b,c ) -> ( a, b |> Maybe.map String.fromFloat |> Maybe.withDefault "Kein Wert vorhanden",c |> Maybe.map String.fromFloat |> Maybe.withDefault "Kein Wert vorhanden" )) ganzerText


umwandeln2 : List ( String, Maybe Float, Maybe Float ) -> List ( String, Float, Float)
umwandeln2 ganzerText =
    List.map (\( a, b,c ) -> ( a, b |>  Maybe.withDefault 0.0 ,c |> Maybe.withDefault 0.0 )) ganzerText



-- UPDATE


renderList : List ( String, String, String ) -> Html msg
renderList lst =
    Html.ul []
        (List.map (\( a, b, c ) -> Html.li [] [ Html.text <| a ++ ", " ++ b ++ ", "++ c ]) lst)


type Msg
    = GotText (Result Http.Error String)
    | GotText2 (Result Http.Error String)
    | MorePlease




update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        currentList =
            case model of
                Success l ->
                    l
                
                Success2 l ->
                    l

                Failure ->
                    []
                
                Loading ->
                    []

           
      
    in
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    ( Success <| currentList ++ [ fullText ], Cmd.none )

                Err _ ->
                    ( model, Cmd.none )
        GotText2 result ->
            case result of
                Ok fullText ->
                    ( Success2 <| currentList ++ [ fullText ], Cmd.none )

                Err _ ->
                    ( model, Cmd.none )
        MorePlease ->
            (Loading, getRandomCatGif2)

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
                [ x 360--(Scale.convert xScaleLocal labelPositions.x)
                , y 35

                -- , fontFamily [ "Helvetica", "sans-serif" ]
                , fontSize (px 20)

                --, fontWeight FontWeightBold
                ]
                [ TypedSvg.Core.text "Age" ]
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
                [ TypedSvg.Core.text "Rating" ]
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



type alias Point =
    { pointName : String, x : Float, y : Float }


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


filterAndReducePlayers : List (String, Float, Float) -> XyData
filterAndReducePlayers my_spielerListe =
    XyData "Rating" "Age" (List.map pointName my_spielerListe)


pointName : (String, Float, Float) -> Point
pointName (x, y, z) =
    Point (x ++ ", Age: " ++ (String.fromFloat y)++ ", Rating: " ++ (String.fromFloat z)) y z






-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Failure ->
            Html.text "I was unable to load your book."

        Loading ->
            Html.text "Loading..."

        Success l ->
            let
                spieler =
                    filterAndReducePlayers <| spielerListe l "Age" "Overall"
                
            in
            Html.div []
                [ Html.button [onClick MorePlease] [ Html.text "More Please!" ]
                
                    
             ,scatterplot spieler
            ]
        
        Success2 l ->
            let
                spieler =
                    filterAndReducePlayers <| spielerListe l "Overall" "Overall"
                
            in
            Html.div []
                [ Html.button [onClick MorePlease] [ Html.text "More Please!" ]
                
                    
             ,scatterplot spieler
            ]
                
    
               



type alias Player =
    { playerName : String
    , age : Float
    , rating : Float
    }

spielerListe : List String -> String -> String -> List(String, Float, Float)--Player    
spielerListe liste1 variable1 variable2 =
 List.map (\fulltext ->  umwandeln2 <| (csvString_to_data fulltext variable1 variable2 )) liste1
    |> List.concat



testListe = 
    testDaten string1 string2 testPoint

testDaten :  String -> String -> List Point -> XyData
testDaten s1 s2 tp=
    XyData s1 s2 tp




string1 =
    "Testdaten1"
string2 =
    "Testdaten2"

testPoint : List Point
testPoint = 
    [Point "Name1" 1.0 2.0
    ,Point  "Name2" 2.0 3.0 ]
    