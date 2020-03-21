module Main exposing (main)

import Browser
import Html
import Http
import Json.Encode
import Roundtrip


type alias Flags =
    ()


type alias Model =
    ()


type Msg
    = Received (Result Http.Error Roundtrip.Value)
    | Returned (Result Http.Error ())


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
        Returned res ->
            case res of
                Err httpError ->
                    Debug.todo (Debug.toString httpError)

                Ok _ ->
                    ( model, Cmd.none )

        Received res ->
            case res of
                Err httpError ->
                    Debug.todo (Debug.toString httpError)

                Ok value ->
                    let
                        json =
                            Debug.log "Outgoing JSON" (Json.Encode.encode 2 (Roundtrip.encodeValue value))
                    in
                    ( model
                    , Roundtrip.postRoundtrip
                        { body = value
                        }
                        |> Cmd.map Returned
                    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
