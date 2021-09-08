module Development.Scatterplot exposing (..)

import Axis
import Browser
import Csv
import Csv.Decode
import Html exposing (Html, a, li, pre, text, ul, input)
import Html.Attributes exposing (placeholder, value, type_, checked)
import Html.Events exposing (onClick, onInput)
import Http
import Scale exposing (ContinuousScale)
import Statistics
import TypedSvg exposing (circle, g, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, fontFamily, fontSize, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), FontWeight(..), Length(..), Transform(..), px)
import Html.Events exposing (onCheck)



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
        , filterNation : String
        , onOff : Bool
        , filterClub : String
        , onOff2 : Bool
        , selectedValue1 : Int
        , selectedValue2 : Int
        }
    


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading 
    , getRandomCatGif GotText
    )


getRandomCatGif : (Result Http.Error String -> Msg) -> Cmd Msg
getRandomCatGif x =
                Http.get
                    { url = "https://raw.githubusercontent.com/JohannesLange/Visualisierung_FIFA19/master/data/spData.csv"
                    , expect = Http.expectString x
                    }




csvString_to_data : String -> List Footballer
csvString_to_data csvRaw =
    Csv.parse csvRaw
        |> Csv.Decode.decodeCsv decodeFootballer
        |> Result.toMaybe
        |> Maybe.withDefault []


type alias Footballer =
    { name : String, age : Float, overall : Float, potential : Float, height : Float, nationality : String, club : String, weight : Float, wage : Float, value : Float}--, bp : String }


decodeFootballer : Csv.Decode.Decoder ( Footballer -> a) a
decodeFootballer =
    Csv.Decode.map Footballer 
        (Csv.Decode.field "Name" Ok 
            |> Csv.Decode.andMap (Csv.Decode.field "Age" (String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "OVA" (String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "POT" (String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "Height" (String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "Nationality" Ok)
            |> Csv.Decode.andMap (Csv.Decode.field "Club" Ok)
            |> Csv.Decode.andMap (Csv.Decode.field "Weight" (String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "Wage" (String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "Value" (String.toFloat >> Result.fromMaybe "error parsing string"))
            --|> Csv.Decode.andMap (Csv.Decode.field "BP" Ok)
        )



-- UPDATE


type Msg
    = GotText (Result Http.Error String)
    | ChangeX (Footballer -> Float, String, Int)
    | ChangeY (Footballer -> Float, String, Int)
    | ChangeNation (String)
    | ChangeClub (String)
    | ActivateFilter Bool
    | ActivateFilter2 Bool



--| UpdateAttribute1 Footballer


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    ( Success <| { data = spielerListe [ fullText ], xAAFunction = .age, yAAFunction = .overall , xName = "Age", yName = "Overall", filterNation = "Argentina", onOff = False, filterClub = "FC Barcelona", onOff2 = False, selectedValue1 = 1, selectedValue2 = 2 }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


        ChangeX (x, a, b) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, xAAFunction = x, yAAFunction = m.yAAFunction, xName = a, yName = m.yName, filterNation = m.filterNation, onOff = m.onOff, filterClub = m.filterClub, onOff2 = m.onOff2, selectedValue1 = b, selectedValue2 = m.selectedValue2 }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ChangeY (y, a, b) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, xAAFunction = m.xAAFunction, yAAFunction = y, xName = m.xName, yName = a, filterNation = m.filterNation, onOff = m.onOff, filterClub = m.filterClub, onOff2 = m.onOff2, selectedValue1 = m.selectedValue1, selectedValue2 = b }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ChangeNation (x) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, xAAFunction = m.xAAFunction, yAAFunction = m.yAAFunction, xName = m.xName, yName = m.yName, filterNation = x, onOff = m.onOff, filterClub = m.filterClub, onOff2 = m.onOff2, selectedValue1 = m.selectedValue1, selectedValue2 = m.selectedValue2}, Cmd.none )

                _ ->
                    ( model, Cmd.none )
        ActivateFilter (x) ->
                        case model of
                Success m ->
                    ( Success <| { data = m.data, xAAFunction = m.xAAFunction, yAAFunction = m.yAAFunction, xName = m.xName, yName = m.yName, filterNation = m.filterNation, onOff = x, filterClub = m.filterClub, onOff2 = m.onOff2, selectedValue1 = m.selectedValue1, selectedValue2 = m.selectedValue2}, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ChangeClub (x) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, xAAFunction = m.xAAFunction, yAAFunction = m.yAAFunction, xName = m.xName, yName = m.yName, filterNation = m.filterNation, onOff = m.onOff, filterClub = x, onOff2 = m.onOff2, selectedValue1 = m.selectedValue1, selectedValue2 = m.selectedValue2}, Cmd.none )

                _ ->
                    ( model, Cmd.none )
        ActivateFilter2 (x) ->
                        case model of
                Success m ->
                    ( Success <| { data = m.data, xAAFunction = m.xAAFunction, yAAFunction = m.yAAFunction, xName = m.xName, yName = m.yName, filterNation = m.filterNation, onOff = m.onOff, filterClub = m.filterClub, onOff2 = x, selectedValue1 = m.selectedValue1, selectedValue2 = m.selectedValue2}, Cmd.none )

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
            .point circle { stroke: rgba(0, 0, 155,0.05); fill: rgba(0, 0, 155,0.1); }
            .point text { display: none; }
            .point:hover circle { stroke: rgba(0, 0, 0,1.0); fill: rgb(118, 214, 78); }
            .point:hover text { display: inline; text-shadow: 2px 2px rgb(255,255,255); fill: rgb(0,0,0); }
            
          """ ]
        , g [ transform [ Translate 60 390 ] ]
            [ xAxis xValues
            , text_
                [ x 360 --(Scale.convert xScaleLocal labelPositions.x)
                , y 35

                , fontFamily [ "sans-serif" ]
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

                , fontFamily [ "sans-serif" ]
                , fontSize (px 20)

                --, fontWeight FontWeightBold
                ]
                [ TypedSvg.Core.text model.yDescription ]
            ]
        , g [ transform [ Translate padding padding ] ]
            
            (List.map (\n -> point xScaleLocal yScaleLocal n model.data) model.data)
        ]


point : ContinuousScale Float -> ContinuousScale Float -> Point  -> List Point -> Svg msg -- List Point
point scaleX scaleY xyPoint pointList=
    g
        [ class [ "point" ]
        , fontSize <| Px 15.0
        , fontFamily [ "sans-serif" ]
        , transform
            [ Translate
                (Scale.convert scaleX xyPoint.x)
                (Scale.convert scaleY xyPoint.y)
            ]
        ]
        [ circle [ cx 0, cy 0, r 3 ] []
        , text_ [ x 10, y -20, textAnchor AnchorMiddle ] [ Html.text xyPoint.pointName ]
        , text_ [ x 10, y -40, textAnchor AnchorMiddle ] [ Html.text <|"Anzahl an Spielern mit diesen X und Y Werten: " ++ String.fromInt(sumX xyPoint pointList) ]
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


filterX : Point -> List Point -> List Point
filterX a b =
    let
        isEqual : Point -> Point -> Maybe Point
        isEqual t z =
            if z.x == t.x && z.y == t.y then 
                Just z

            else
                Nothing
    in
    List.filterMap (isEqual a) b  



sumX : Point -> List Point ->  Int
sumX e f=
    List.length (filterX e f) 

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


filterAttribute : List Footballer -> (Footballer -> String) -> String -> List Footballer
filterAttribute spieler attribut wort =
    List.filter (\n -> (attribut n) == wort ) spieler




     


view : Model -> Html Msg
view model =
    case model of
        Failure ->
            Html.text "I was unable to load the players."

        Loading  ->
            Html.text "Loading Players..."


        Success l ->
            let 

                v1 =
                    if l.selectedValue1 == 1 then 
                        "✓ "
                    else 
                        ""
                v2 =
                    if l.selectedValue1 == 2 then 
                        "✓ "
                    else 
                        ""
                v3 =
                    if l.selectedValue1 == 3 then 
                        "✓ "
                    else 
                        ""

                v4 =
                    if l.selectedValue1 == 4  then 
                        "✓ "
                    else 
                        ""
                v5 =
                    if l.selectedValue1 == 5  then 
                        "✓ "
                    else 
                        ""
                v6 =
                    if l.selectedValue1 == 6  then 
                        "✓ "
                    else 
                        ""
                v21 =
                    if l.selectedValue2 == 1 then 
                        "✓ "
                    else 
                        ""
                v22 =
                    if l.selectedValue2 == 2 then 
                        "✓ "
                    else 
                        ""
                v23 =
                    if l.selectedValue2 == 3 then 
                        "✓ "
                    else 
                        ""

                v24 =
                    if l.selectedValue2 == 4  then 
                        "✓ "
                    else 
                        ""
                v25 =
                    if l.selectedValue2 == 5  then 
                        "✓ "
                    else 
                        ""
                v26 =
                    if l.selectedValue2 == 6  then 
                        "✓ "
                    else 
                        ""

                spieler2 =
                    filterAttribute l.data .nationality l.filterNation
                spieler3 =
                    filterAttribute l.data .club l.filterClub
                spieler4 = 
                    filterAttribute spieler2 .club l.filterClub
                spielerfiltered =
                   if l.onOff == True && l.onOff2 == True then 
                        filterAndReducePlayers spieler4 .name l.xAAFunction l.yAAFunction l.xName l.yName
                    else if l.onOff == True && l.onOff2 == False then
                        filterAndReducePlayers spieler2 .name l.xAAFunction l.yAAFunction l.xName l.yName
                    else if l.onOff == False && l.onOff2 == True then
                        filterAndReducePlayers spieler3 .name l.xAAFunction l.yAAFunction l.xName l.yName
                    else 
                        filterAndReducePlayers l.data .name l.xAAFunction l.yAAFunction l.xName l.yName
                spieler =
                    filterAndReducePlayers l.data .name l.xAAFunction l.yAAFunction l.xName l.yName
            in
            Html.div []
                [
                ul [][
                Html.text <| "Da der Datensatz sehr groß ist kann das Textfeld genutzt werden um nach Nationen zu filtern."]
                ,ul[][
                Html.text <| "Um dies zu aktivieren muss die Box angeklickt werden."]
                ,ul[][  
                input [ placeholder "Select Nation to Filter", value l.filterNation, onInput ChangeNation ] []
                ,input [ type_ "checkbox", onCheck ActivateFilter ] []
                ,input [ placeholder "Select Club to Filter", value l.filterClub, onInput ChangeClub ] []
                ,input [ type_ "checkbox", onCheck ActivateFilter2 ] []]
                
                 ,ul []
                    [ 
                            Html.text <| "X Variable festlegen: "
                            , Html.button [ onClick (ChangeX (.overall, "Overall", 1)) ] [ Html.text (v1 ++ "Overall") ]
                            , Html.button [ onClick (ChangeX (.potential, "Potential", 2)) ] [ Html.text (v2 ++"Potential") ]
                            , Html.button [ onClick (ChangeX (.age, "Age", 3)) ] [ Html.text (v3 ++"Age") ]
                            , Html.button [ onClick (ChangeX (.height, "Height", 4)) ] [ Html.text (v4 ++ "Height") ]
                            , Html.button [ onClick (ChangeX (.wage, "Wage", 5)) ] [ Html.text (v5 ++ "Wage in Tausend") ]
                            , Html.button [ onClick (ChangeX (.value, "Value", 6)) ] [ Html.text (v6 ++ "Value in Tausend") ]
                    ]
                , ul []
                    [ 
                            Html.text <| "Y Variable festlegen: "
                            , Html.button [ onClick (ChangeY (.overall, "Overall", 1)) ] [ Html.text (v21 ++"Overall") ]
                            , Html.button [ onClick (ChangeY (.potential, "Potential", 2)) ] [ Html.text (v22 ++"Potential") ]
                            , Html.button [ onClick (ChangeY (.age, "Age", 3)) ] [ Html.text (v23 ++ "Age") ]
                            , Html.button [ onClick (ChangeY (.height, "Height",4)) ] [ Html.text (v24 ++"Height") ]
                            , Html.button [ onClick (ChangeY (.wage, "Wage", 5)) ] [ Html.text (v25 ++ "Wage in Tausend") ]
                            , Html.button [ onClick (ChangeY (.value, "Value", 6)) ] [ Html.text (v26 ++ "Value in Tausend") ]
                    ]
                           
                --  , Html.input [ Html.Attributes.placeholder "Age", Html.Attributes.value model.content, onInput Change ]
                , scatterplot spielerfiltered
                , ul[][ Html.text <| "Legende:"]
                , ul [][Html.text <| "Beispielnationen: Germany, England, France, Portugal"]    
                ]
