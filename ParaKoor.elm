module Main exposing (main)

import Axis
import Browser
import Color
import Html exposing (Html, a, li, ul)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import List.Extra
import Path
import Scale exposing (ContinuousScale)
import Shape
import Statistics
import TypedSvg exposing (circle, g, line, path, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, d, fill, fontFamily, fontSize, stroke, strokeWidth, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Paint(..), Transform(..))


selectedCarType : { carType : CarType, carString : String }
selectedCarType =
    --    ( Sports_Car, "Sport Cars" )
    --    ( SUV, "SUVs" )
    --    ( Wagon, "Wagons" )
    --    ( Pickup, "Pickups" )
    { carType = Minivan, carString = "Minivans" }


type alias Model =
    { zahlCityMPG : Int
    , attributeValue : Int
    , accessWerte : List ( String, FilteredCar -> Int )
    , accessWerte2 : List ( String, FilteredCar -> Int )
    , value1 : String
    , value2 : String
    , value3 : String
    , value4 : String
    }


type Msg
    = CityMPGOben
    | CityMPGUnten
    | AttributeCityMPG
    | AttributeDealerCost
    | AttributeRetailPrice
    | AttributeCarLen
    | Tausch1
    | Tausch2
    | Tausch3


initialModel : Model
initialModel =
    { zahlCityMPG = 0
    , attributeValue = 0
    , accessWerte =
        [ ( "CityMPG", .cityMPG )
        , ( "dealerCost", .dealerCost )
        , ( "retailPrice", .retailPrice )
        , ( "carLen", .carLen )
        ]
    , accessWerte2 =
        [ ( "CityMPG", .cityMPG )
        , ( "dealerCost", .dealerCost )
        , ( "retailPrice", .retailPrice )
        , ( "carLen", .carLen )
        ]
    , value1 = "City MPG" 
    , value2 = "Dealer Cost"
    , value3 = "Retail Price" 
    , value4 = "Car Length"    
    }


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }


update : Msg -> Model -> Model
update msg model =
    case msg of
        CityMPGOben ->
            { model | zahlCityMPG = model.zahlCityMPG + 1 }

        Tausch1 ->
            { model | accessWerte2 = List.Extra.swapAt 0 1 model.accessWerte2, value1 = model.value2, value2 = model.value1 }
        
        Tausch2 ->
            { model | accessWerte2 = List.Extra.swapAt 1 2 model.accessWerte2, value2 = model.value3, value3 = model.value2  }

        Tausch3 ->
            { model | accessWerte2 = List.Extra.swapAt 2 3 model.accessWerte2, value3 = model.value4, value4 = model.value3 }
        


        CityMPGUnten ->
            { model | zahlCityMPG = model.zahlCityMPG - 1 }

        AttributeDealerCost ->
            { model | attributeValue = 1 }

        AttributeCityMPG ->
            { model | attributeValue = 0 }

        AttributeCarLen ->
            { model | attributeValue = 3 }

        AttributeRetailPrice ->
            { model | attributeValue = 2 }


view : Model -> Html Msg
view model =
    let
        filteredCars : List FilteredCar
        filteredCars =
            filterMissingValues cars
                |> List.filter
                    (.carType
                        >> (==) selectedCarType.carType
                    )

        numberFilteredCars =
            List.length filteredCars |> String.fromInt

        multiDimDaten =
            MultiDimData (List.map Tuple.first (my_access_function model))
                [ List.map
                    (\x ->
                        List.map (\accessFunction -> Tuple.second accessFunction x) (my_access_function model)
                            --[ my_access_function  ]
                            |> List.map toFloat
                            |> MultiDimPoint x.vehicleName
                    )
                    filteredCars
                ]
    in
    Html.div []
        [ Html.text <| selectedCarType.carString
        , ul []
            [ li [] [ Html.text <| "Number of filtered Cars: " ++ numberFilteredCars ]
            ]
        , ul []
            [ Html.button [ onClick Tausch1 ] [ Html.text <| "Tausche " ++ model.value1 ++" und " ++ model.value2 ]
            , Html.button [ onClick Tausch2 ] [ Html.text <| "Tausche " ++ model.value2 ++" und " ++ model.value3 ]
            , Html.button [ onClick Tausch3 ] [ Html.text <| "Tausche " ++ model.value3 ++" und " ++ model.value4 ]
            ]
        , parallelCoodinatesPlot 600 2 multiDimDaten
        ]


type alias MultiDimPoint =
    { pointName : String, value : List Float }


type alias MultiDimData =
    { dimDescription : List String
    , data : List (List MultiDimPoint)
    }


type alias FilteredCar =
    { vehicleName : String, carType : CarType, cityMPG : Int, retailPrice : Int, dealerCost : Int, carLen : Int }


my_access_function : Model -> List ( String, FilteredCar -> Int )
my_access_function model =
            List.Extra.swapAt model.attributeValue model.zahlCityMPG model.accessWerte2




--accessWerte =
--        [ ( "CityMPG", .cityMPG )
--        , ( "dealerCost", .dealerCost )
--        , ( "retailPrice", .retailPrice )
--        , ( "carLen", .carLen )
--        ]
--type alias AccessFunction =
--    { attributeName : String, attributeWert : FilteredCar -> Int)
--accessWerte : List AccessFunction


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


filterMissingValues : List Car -> List FilteredCar
filterMissingValues my_cars =
    let
        car2filteredCar : Car -> Maybe FilteredCar
        car2filteredCar car =
            Maybe.map4 (FilteredCar car.vehicleName car.carType)
                car.cityMPG
                car.retailPrice
                car.dealerCost
                car.carLen
    in
    List.filterMap car2filteredCar my_cars


type CarType
    = Small_Sporty_Compact_Large_Sedan
    | Sports_Car
    | SUV
    | Wagon
    | Minivan
    | Pickup


type WheelDrive
    = All_Wheel_Drive
    | Rear_Wheel_Drive
    | Front_Wheel_Drive


type alias Car =
    { vehicleName : String
    , carType : CarType
    , wheelDrive : WheelDrive
    , retailPrice : Maybe Int
    , dealerCost : Maybe Int
    , engineSize : Maybe Float
    , cyl : Maybe Float
    , hp : Maybe Int
    , cityMPG : Maybe Int
    , hwyMPG : Maybe Int
    , weight : Maybe Int
    , wheelBase : Maybe Int
    , carLen : Maybe Int
    , carWidth : Maybe Int
    }


cars : List Car
cars =
    [ Car "Acura 3.5 RL 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 43755) (Just 39014) (Just 3.5) (Just 6) (Just 225) (Just 18) (Just 24) (Just 3880) (Just 115) (Just 197) (Just 72)
    , Car "Acura 3.5 RL w/Navigation 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 46100) (Just 41100) (Just 3.5) (Just 6) (Just 225) (Just 18) (Just 24) (Just 3893) (Just 115) (Just 197) (Just 72)
    , Car "Acura MDX" SUV All_Wheel_Drive (Just 36945) (Just 33337) (Just 3.5) (Just 6) (Just 265) (Just 17) (Just 23) (Just 4451) (Just 106) (Just 189) (Just 77)
    , Car "Acura NSX coupe 2dr manual S" Sports_Car Rear_Wheel_Drive (Just 89765) (Just 79978) (Just 3.2) (Just 6) (Just 290) (Just 17) (Just 24) (Just 3153) (Just 100) (Just 174) (Just 71)
    , Car "Acura RSX Type S 2dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 23820) (Just 21761) (Just 2) (Just 4) (Just 200) (Just 24) (Just 31) (Just 2778) (Just 101) (Just 172) (Just 68)
    , Car "Acura TL 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 33195) (Just 30299) (Just 3.2) (Just 6) (Just 270) (Just 20) (Just 28) (Just 3575) (Just 108) (Just 186) (Just 72)
    , Car "Acura TSX 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 26990) (Just 24647) (Just 2.4) (Just 4) (Just 200) (Just 22) (Just 29) (Just 3230) (Just 105) (Just 183) (Just 69)
    ]
