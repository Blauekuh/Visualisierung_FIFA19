module Development.Tree01 exposing (..)

import Axis
import Browser
import Color
import Csv
import Csv.Decode
import Json.Decode  as Decode
import Json.Encode as Encode exposing (bool, encode, int, list, string)
import TreeDiagram exposing (node, TreeOrientation, topToBottom)
import TreeDiagram.Svg
import Html exposing (Html, a, li, pre, text, ul, input)
import Html.Attributes exposing (placeholder, value, type_, checked)
import Html.Events exposing (onClick, onInput, onCheck)
import Http
import Scale exposing (ContinuousScale)
import Statistics
import TypedSvg exposing (circle, g, line, path, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, d, fill, fontFamily, fontSize, stroke, strokeWidth, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg)--, text)
import TypedSvg.Types as ST exposing (AnchorAlignment(..), Length(..), Paint(..), Transform(..))



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
         ,xName : String
         ,liga : List String
         ,position : String
         ,onOff : Bool
        }
    


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading 
    , getRandomCatGif GotText
    )


getRandomCatGif : (Result Http.Error String -> Msg) -> Cmd Msg
getRandomCatGif x =
                Http.get
                    { url = "https://raw.githubusercontent.com/JohannesLange/Visualisierung_FIFA19/master/data/baumData.csv"
                    , expect = Http.expectString x
                    }




--liste : List String
--liste =
--    [ "data1000.csv" ]


csvString_to_data : String -> List Footballer
csvString_to_data csvRaw =
    Csv.parse csvRaw
        |> Csv.Decode.decodeCsv decodeFootballer
        |> Result.toMaybe
        |> Maybe.withDefault []


type alias Footballer =
    { name : String, club : String, position: String}-- , age : String}--, liga : String }-- age : String, overall : String, potential : String, height : String }

{--
decodeFootballer : Csv.Decode.Decoder ( Footballer -> a) a
decodeFootballer =
    Csv.Decode.map Footballer 
        (Csv.Decode.field "Name" Ok 
            |> Csv.Decode.andMap (Csv.Decode.field "Age" (String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "Overall" (String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "Potential" (String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "Height" (String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "Club" Ok)
        )
--}
decodeFootballer : Csv.Decode.Decoder ( Footballer -> a) a
decodeFootballer =
    Csv.Decode.map Footballer 
        (Csv.Decode.field "Name" Ok 
            --|> Csv.Decode.andMap (Csv.Decode.field "Age" Ok)
            --|> Csv.Decode.andMap (Csv.Decode.field "Overall" Ok)
            --|> Csv.Decode.andMap (Csv.Decode.field "Potential" Ok)
            --|> Csv.Decode.andMap (Csv.Decode.field "Height"  Ok)
            |> Csv.Decode.andMap (Csv.Decode.field "Club" Ok)
            |> Csv.Decode.andMap (Csv.Decode.field "BP" Ok)
        )

-- UPDATE


type Msg
    = GotText (Result Http.Error String)
    | ChangeLiga (String, List String)
    | ChangePosition (String)
    | ActivateFilter (Bool)


--| UpdateAttribute1 Footballer


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    ( Success <| { data = spielerListe [ fullText ], xName = "Calcio A", liga = listeCalcio, position = "ST", onOff = False  }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )
        ChangeLiga(x, y) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, xName = x, liga = y, position = m.position, onOff = m.onOff}, Cmd.none )

                _ ->
                    ( model, Cmd.none )
        ChangePosition(x) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, xName = m.xName, liga = m.liga, position = x, onOff = m.onOff}, Cmd.none )

                _ ->
                    ( model, Cmd.none )
        ActivateFilter(x) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, xName = m.xName, liga = m.liga, position = m.position, onOff = x}, Cmd.none )

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




drawLine : ( Float, Float ) -> Svg msg
drawLine ( targetX, targetY ) =
    line
        [ x1 0, y1 0, x2 targetX, y2 targetY, stroke (ST.Paint Color.black) ]
        []


{-| Represent nodes as circles with the node value inside.
-}
drawNode : String -> Svg msg
drawNode n =

    g
        []
        [ circle 
            [ r 16
            , stroke (Paint Color.black)
            , fill (Paint Color.white)
            , cx 0
            , cy 0
            
            ] 
            []
        , text_ 
            [ textAnchor AnchorStart
            , transform 
                [ Translate 5.5 20.5 
                , Rotate 25.0 0.0 0.0
                ]
            ] 
            [ text n ]
        ]
   

type alias Liga =
    {name : String
    , children : List Mannschaft}

type alias Mannschaft =
    { name : String
    , children : List Footballer
    , liga : String
    , anzahl: Int
    }

type alias Spieler =
    {name : String
    , age : String
    }

type alias Ligen =
    {name : String
    ,team : String}


ligenListe : List Ligen
ligenListe =
    [Ligen "Premier League" "Chelsea"
    ,Ligen "Premier League" "Tottenham"
    ,Ligen "Calcio A" "Juventus"]

--clubCombiner : List String -> List Footballer ->List Mannschaft
--clubCombiner x y =
--    List.map (\n -> Mannschaft n (List.filterMap (\b ->b.club == n)y )) x

clubCombiner : String -> List Footballer -> String -> Mannschaft
clubCombiner x y z =
        Mannschaft x (List.filter (\b -> b.club == x)y) z (List.length (List.filter (\b -> b.club == x)y) )--(y.liga)

ligenCombiner : String -> List String -> List Footballer -> Liga
ligenCombiner x y z =
    Liga x (List.filter (\b -> b.liga == x) (List.map (\n-> clubCombiner n z x) y)) 

ligenEncoder : Liga -> Encode.Value    
ligenEncoder liga =
    Encode.object
        [ ( "name", Encode.string liga.name )
        , ( "children", Encode.list mannschaftsEncoder liga.children)
        ]

mannschaftsEncoder : Mannschaft -> Encode.Value
mannschaftsEncoder team =
    Encode.object
        [ ( "name", Encode.string team.name )
        , ( "children", Encode.list spielerEncoder team.children)
        , ( "anzahl", Encode.int team.anzahl)
        ]

spielerEncoder : Footballer -> Encode.Value
spielerEncoder spieler = 
    Encode.object
        [ ( "name", Encode.string spieler.name )
        --, ( "name", Encode.string spieler.age )
        , ( "club", Encode.string spieler.club)
        --, ( "position", Encode.string spieler.position )
        --, ( "potential", Encode.string spieler.potential)
        --, ( "height", Encode.string spieler.height)

        ]
        
testEncoder x y a =
    Encode.encode 0 (ligenEncoder (ligenCombiner a x y))
--testEncoder x y =
--    Encode.encode 0 (mannschaftsEncoder (clubCombiner x y))

treeDecoder2 : Decode.Decoder (TreeDiagram.Tree String)
treeDecoder2 =
    Decode.map3
        (\name children anzahl->
            case children of
                Nothing ->
                    case anzahl of 
                        Nothing ->
                            TreeDiagram.node (name) []
                        Just a ->
                            TreeDiagram.node (String.fromInt(a)) []

                Just c ->
                    case anzahl of 
                        Nothing ->
                            TreeDiagram.node (name) c
                        Just a ->
                            TreeDiagram.node (name++ ", Anzahl Spieler im Team: " ++String.fromInt(a)) c
        )               
        (Decode.field ("name") Decode.string) 
        (Decode.maybe <|
            Decode.field "children"<|
                Decode.list <|
                    Decode.lazy
                        (\_ -> treeDecoder2)
        )
        (Decode.maybe <|Decode.field "anzahl" Decode.int)

{--
treeDecoder2 : Decode.Decoder (TreeDiagram.Tree String)
treeDecoder2 =
    Decode.map4
        (\name children anzahl overall->
            case children of
                Nothing ->
                    case anzahl of 
                        Nothing ->
                            case overall of 
                                Nothing ->
                                    TreeDiagram.node (name) []
                                Just b ->
                                    TreeDiagram.node (name++ b) []
                            
                        Just a ->
                            case overall of 
                                Nothing ->
                                    TreeDiagram.node (name) []
                                Just b ->
                                    TreeDiagram.node (name ++ b) []

                Just c ->
                    case anzahl of 
                        Nothing ->
                            case overall of 
                                Nothing ->
                                    TreeDiagram.node (name) c
                                Just b ->
                                    TreeDiagram.node (name++ b) c
                            
                        Just a ->
                            case overall of 
                                Nothing ->
                                    TreeDiagram.node (name++ "T") c
                                Just b ->
                                    TreeDiagram.node (name ++ b) c
                        --Just a ->
                          --  TreeDiagram.node (name++ ", Anzahl Spieler im Team: " ++String.fromInt(a)) c
        )               
        (Decode.field ("name") Decode.string) 
        (Decode.maybe <|
            Decode.field "children"<|
                Decode.list <|
                    Decode.lazy
                        (\_ -> treeDecoder2)
        )
        (Decode.maybe <|Decode.field "anzahl" Decode.int)
        (Decode.maybe <|Decode.field "age" Decode.string)

--}
listeCalcio : List String        
listeCalcio =
    ["Juventus",
     "Napoli",
     "Milan",
     "Inter",
     "Lazio",
     "Roma"]


listePremier : List String
listePremier =
    ["Chelsea",
     "West Ham United"
     
     ]

listeBundes : List String
listeBundes =
    ["VfL Wolfsburg", 
    "Bayer 04 Leverkusen", 
    "SC Freiburg", 
    "1. FC Köln", 
    "1. FSV Mainz 05", 
    "1. FC Union Berlin", 
    "Hertha BSC", 
    "TSG 1899 Hoffenheim", 
    "RB Leipzig", 
    "Borussia Mönchengladbach", 
    "Eintracht Frankfurt", 
    "DSC Arminia Bielefeld", 
    "SV Werder Bremen", 
    "FC Schalke 04", 
    "FC Augsburg", 
    "Borussia Dortmund",
    "FC Bayern München",
    "VfB Stuttgart"]

listeBbva : List String
listeBbva =
    ["Real Madrid",
     "FC Barcelona",
     "Athletic Club de Bilbao",
     "Atlético Madrid",
     "Real Betis",
     "CA Osasuna",
     "RC Celta",
     "Deportivo Alavés",
     "Cádiz CF",
     "Elche CF",
     "Getafe CF",
     "Sevilla FC",
     "Valencia CF",
     "Villarreal CF",
     "Granada CF",
     "Real Sociedad",
     "Real Valladolid CF",
     "SD Eibar",
     "SD Huesca",
     "Levante UD"
     ]



type alias TreeLayout =
    { orientation : TreeOrientation
    , levelHeight : Int
    , subtreeDistance : Int
    , siblingDistance : Int
    , padding : Int
    }



newTreelayout =
    TreeLayout topToBottom 250 80 80 120


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


                spielerfiltered =
                   if l.onOff == True then
                    filterAttribute l.data .position l.position 
                    else
                        l.data

                premierL =
                    testEncoder l.liga spielerfiltered  l.xName
                    --testEncoder (List.filter (\n-> List.member n listeCalcio)listeCalcio) l.data  "Calcio A"--listSpieler -- l.data
            in
                case Decode.decodeString treeDecoder2 premierL of
                    Ok schools ->
                        Html.div []
                            [   
                            ul [][
                                Html.text <| "Um dies zu aktivieren muss die Box angeklickt werden (Wenn eine neue Nation eingegeben wird, die Checkbox einmal deaktivieren und wieder reaktivieren)."]
                            ,ul[][  
                                input [ placeholder "Select Position to filter", value l.position, onInput ChangePosition ] []
                                ,input [ type_ "checkbox", onCheck ActivateFilter ] []]
                            ,ul[][    
                                 li [] [
                                    Html.text <| "Set X Value"
                                    , Html.button [ onClick (ChangeLiga ("Calcio A",listeCalcio)) ] [ Html.text "Calcio A" ]
                                    , Html.button [ onClick (ChangeLiga ("Premier League",listePremier)) ] [ Html.text "Premier League" ]
                                    , Html.button [ onClick (ChangeLiga ("Bundesliga",listeBundes)) ] [ Html.text "Bundesliga" ]
                                    , Html.button [ onClick (ChangeLiga ("La Liga",listeBbva)) ] [ Html.text "La Liga" ]
                                        ]
                                ]   
                            ,ul[][
                             TreeDiagram.Svg.draw newTreelayout drawNode drawLine schools]
                            ]
                        

                    Err error ->
                        Html.text ("Error: " ++ Decode.errorToString error)


