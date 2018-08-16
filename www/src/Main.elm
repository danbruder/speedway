module Main exposing (..)

import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src)
import AnimationFrame
import Time exposing (Time, second)
import Keyboard


---- MODEL ----


type alias Model =
    { car : Car
    , time : Time
    , pedal : Pedal
    }


init : ( Model, Cmd Msg )
init =
    ( { car = Car Red ( 0, 0 ) 0 0
      , time = 0
      , pedal = PedalNotPressed
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Tick Time
    | KeyDownMsg Keyboard.KeyCode
    | KeyUpMsg Keyboard.KeyCode
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDownMsg code ->
            case code |> controlFromCode of
                Right ->
                    let
                        newModel =
                            model.car.rotation
                                + rotationSpeed
                                |> asRotationIn model.car
                                |> asCarIn model
                    in
                        ( newModel, Cmd.none )

                Left ->
                    let
                        newModel =
                            model.car.rotation
                                - rotationSpeed
                                |> asRotationIn model.car
                                |> asCarIn model
                    in
                        ( newModel, Cmd.none )

                Space ->
                    ( { model | pedal = PedalPressed }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        KeyUpMsg code ->
            case code |> controlFromCode of
                Space ->
                    ( { model | pedal = PedalNotPressed }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Tick newTime ->
            model
                |> updateFromTick msg

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
        div []
            [ img
                [ src "http://clipartriver.com/wp-content/uploads/2017/11/top-view-car-clipart-images-school-pics-free-000.png"
                , css
                    [ transforms
                        [ translate3d (px x) (px y) (px 0)
                        , rotateZ (deg rotation)
                        ]
                    ]
                ]
                []
            , div []
                []
            ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Tick
        , Keyboard.downs KeyDownMsg
        , Keyboard.ups KeyUpMsg
        ]



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


type Pedal
    = PedalPressed
    | PedalNotPressed


type Controls
    = Left
    | Right
    | Space
    | None


controlFromCode : Keyboard.KeyCode -> Controls
controlFromCode code =
    case code of
        37 ->
            Left

        39 ->
            Right

        32 ->
            Space

        _ ->
            None


setRotation : Rotation -> Car -> Car
setRotation rotation car =
    { car | rotation = rotation }


asRotationIn : Car -> Rotation -> Car
asRotationIn =
    flip setRotation


setSpeed : Speed -> Car -> Car
setSpeed speed car =
    { car | speed = speed }


asSpeedIn : Car -> Speed -> Car
asSpeedIn =
    flip setSpeed


setCar : Car -> Model -> Model
setCar car model =
    { model | car = car }


asCarIn : Model -> Car -> Model
asCarIn =
    flip setCar


rotationSpeed : Float
rotationSpeed =
    8


maxSpeed : Float
maxSpeed =
    5


updateFromTick : Msg -> Model -> ( Model, Cmd Msg )
updateFromTick msg model =
    let
        newModel =
            model
                |> updateSpeed
                |> updateLocation
    in
        ( newModel, Cmd.none )


updateSpeed : Model -> Model
updateSpeed model =
    case model.pedal of
        PedalPressed ->
            let
                newSpeed =
                    if model.car.speed < maxSpeed then
                        model.car.speed + 1
                    else
                        model.car.speed

                newModel =
                    newSpeed
                        |> asSpeedIn model.car
                        |> asCarIn model
            in
                newModel

        PedalNotPressed ->
            let
                car =
                    model.car

                speed =
                    car.speed

                newSpeed =
                    if speed > 0 then
                        speed - 0.7
                    else
                        0
            in
                { model | car = { car | speed = newSpeed } }


updateLocation : Model -> Model
updateLocation model =
    let
        ( currentX, currentY ) =
            model.car.location

        speed =
            model.car.speed

        angle =
            model.car.rotation * pi / 180

        newX =
            currentX + speed * (angle |> cos)

        newY =
            currentY + speed * (angle |> sin)

        newPos =
            ( newX, newY )

        { car } =
            model

        newCar =
            { car | location = newPos }
    in
        { model | car = newCar }
