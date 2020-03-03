module Void exposing (..)

import Http
import Json.Decode
import Json.Encode
import Url.Builder


getWish : {} -> Cmd (Result Http.Error EitherVoidUnicorn)
getWish {} =
    Http.request
        { tracker = Nothing
        , timeout = Nothing
        , expect = Http.expectJson identity decoderEitherVoidUnicorn
        , body = Http.emptyBody
        , url = Url.Builder.absolute [ "wish" ] (List.concat [])
        , headers = []
        , method = "GET"
        }


type EitherVoidUnicorn
    = Left Never
    | Right Unicorn


encodeEitherVoidUnicorn : EitherVoidUnicorn -> Json.Encode.Value
encodeEitherVoidUnicorn eitherVoidUnicorn =
    case eitherVoidUnicorn of
        Left param1 ->
            Json.Encode.list identity [ never param1 ]

        Right param1 ->
            Json.Encode.list identity [ encodeUnicorn param1 ]


decoderEitherVoidUnicorn : Json.Decode.Decoder EitherVoidUnicorn
decoderEitherVoidUnicorn =
    Json.Decode.field "ctor" Json.Decode.string
        |> Json.Decode.andThen
            (\ctor ->
                Json.Decode.field "val" <|
                    case ctor of
                        "Left" ->
                            Json.Decode.fail
                                "Cannot decode Never type from JSON"
                                |> Json.Decode.map Left

                        "Right" ->
                            decoderUnicorn
                                |> Json.Decode.map Right

                        _ ->
                            Json.Decode.fail "Unexpected constructor"
            )


type alias Unicorn =
    Never


encodeUnicorn : Unicorn -> Json.Encode.Value
encodeUnicorn =
    never


decoderUnicorn : Json.Decode.Decoder Unicorn
decoderUnicorn =
    Json.Decode.fail "Cannot decode Never type from JSON"
