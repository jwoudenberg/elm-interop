module Main exposing (main)

import Browser
import Html
import Http
import Roundtrip


type alias Flags =
    ()


type alias Model =
    ()


type Msg
    = Received (Result Http.Error Roundtrip.Value)
    | NoOp


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( ()
    , Roundtrip.getRoundtrip {}
        |> Cmd.map Received
    )


view : Model -> Html.Html Msg
view _ =
    Html.text ""


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Received res ->
            ( model
            , Roundtrip.postRoundtrip
                { body =
                    case res of
                        Err httpError ->
                            Roundtrip.Left (Debug.toString httpError)

                        Ok value ->
                            Roundtrip.Right value
                }
                |> Cmd.map (\_ -> NoOp)
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
