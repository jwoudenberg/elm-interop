module Main exposing (main)

import Browser
import Html
import Http
import Roundtrip


type alias Flags =
    ()


type alias Model =
    ()


type alias Msg =
    Result Http.Error Roundtrip.Value


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
    )


view : Model -> Html.Html Msg
view _ =
    Html.text ""


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model
    , Roundtrip.postRoundtrip
        { body =
            case msg of
                Err httpError ->
                    Roundtrip.Left (Debug.toString httpError)

                Ok value ->
                    Roundtrip.Right value
        }
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
