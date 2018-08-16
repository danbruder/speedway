module Main exposing (..)

import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src)
import Time exposing (Time, second)


---- MODEL ----


type alias Model =
    { car : Car
    , time : Time
    }


init : ( Model, Cmd Msg )
init =
    ( { car = Car Red ( 100, 100 ) 34 10
      , time = 0
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Tick Time
    | KeyDown Int
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown key ->
            ( model, Cmd.none )

        Tick newTime ->
            let
                ( currentX, currentY ) =
                    model.car.location

                newX =
                    currentX + model.car.speed * cos (model.car.rotation)

                newY =
                    currentY + model.car.speed * sin (model.car.rotation)

                newPos =
                    ( newX, newY )

                newRotation =
                    model.car.rotation

                { car } =
                    model

                newCar =
                    { car | location = newPos, rotation = newRotation }
            in
                ( { model | car = newCar }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        ( x, y ) =
            model.car.location

        rotation =
            model.car.rotation
    in
        div [ onKeyDown KeyDown ]
            [ div
                [ css
                    [ width (px 100)
                    , height (px 200)
                    , backgroundColor (rgb 123 123 123)
                    , transforms
                        [ translate3d (px x) (px y) (px 0)
                        , rotateZ (deg rotation)
                        ]
                    ]
                ]
                []
            ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view >> toUnstyled
        , init = init
        , update = update
        , subscriptions = subscriptions
        }



---- Local Shit ----


type alias Car =
    { color : Color
    , location : Coordinate
    , rotation : Rotation
    , speed : Speed
    }


type Color
    = Red
    | Blue
    | Purple


type alias Coordinate =
    ( Float, Float )


type alias Rotation =
    Float


type alias Speed =
    Float
