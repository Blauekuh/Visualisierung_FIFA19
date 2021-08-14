module Development.Tree01 exposing (..)

import Browser
import Color
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Http
import Json.Decode
import TreeDiagram
import TreeDiagram.Svg
import TypedSvg exposing (circle, g, line, path, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, d, fill, fontFamily, fontSize, stroke, strokeWidth, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types as ST exposing (AnchorAlignment(..), Length(..), Paint(..), Transform(..))


-- Tree to draw


type alias Model =
    { tree : TreeDiagram.Tree String, errorMsg : String }

init : () -> ( Model, Cmd Msg )
init () =
    ( { tree = TreeDiagram.node "" [], errorMsg = "Loading ..." }
    , Http.get { url = "https://users.informatik.uni-halle.de/~hinnebur/Lehre/InfoVis/U07/flare.json", expect = Http.expectJson GotFlare treeDecoder }
    )


type Msg
    = GotFlare (Result Http.Error (TreeDiagram.Tree String))


treeDecoder : Json.Decode.Decoder (TreeDiagram.Tree String)
treeDecoder =
    Json.Decode.map2
        (\name children ->
            case children of
                Nothing ->
                    TreeDiagram.node name []

                Just c ->
                    TreeDiagram.node name c
        )
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.maybe <|
            Json.Decode.field "children" <|
                Json.Decode.list <|
                    Json.Decode.lazy
                        (\_ -> treeDecoder)
        )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotFlare (Ok newTree) ->
            ( { model | tree = newTree, errorMsg = "No Error" }, Cmd.none )

        GotFlare (Err error) ->
            ( { model
                | tree = TreeDiagram.node "" []
                , errorMsg =
                    case error of
                        Http.BadBody newErrorMsg ->
                            newErrorMsg

                        _ ->
                            "Some other Error"
              }
            , Cmd.none
            )
{-| Represent edges as straight lines.
-}
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
                , Rotate 60.0 0.0 0.0
                ]
            ] 
            [ text (n++"Test") ]
        ]

view : Model -> Html Msg
view model =
    div []
        [ TreeDiagram.Svg.draw TreeDiagram.defaultTreeLayout drawNode drawLine model.tree --Html.text model.errorMsg
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \m -> Sub.none
        }