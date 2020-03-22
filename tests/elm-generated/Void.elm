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
        , expect =
            Http.expectJson
                identity
                (Json.Decode.lazy (\_ -> decoderEitherVoidUnicorn))
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
        Left param ->
            Json.Encode.object
                [ ( "ctor", Json.Encode.string "Left" )
                , ( "val", never param )
                ]

        Right param ->
            Json.Encode.object
                [ ( "ctor", Json.Encode.string "Right" )
                , ( "val", encodeUnicorn param )
                ]


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
                            Json.Decode.lazy (\_ -> decoderUnicorn)
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
