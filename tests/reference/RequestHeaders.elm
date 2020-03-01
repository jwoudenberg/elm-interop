module RequestHeaders exposing (..)

import Http
import Json.Decode
import Json.Encode


getSecret :
    { password : Password
    , fingerprint : Float
    , voiceprint : Float
    }
    -> Cmd (Result Http.Error Secret)
getSecret { password, fingerprint, voiceprint } =
    Http.request
        { tracker = Nothing
        , timeout = Nothing
        , expect = Http.expectJson identity decoderSecret
        , body = Http.emptyBody
        , url =
            String.concat
                [ "http://example.com/"
                , "secret"
                , "?"
                , []
                    |> List.intersperse "&"
                    |> String.concat
                ]
        , headers =
            [ Http.header "password" ((\(Password string) -> string) password)
            , Http.header "fingerprint" (String.fromFloat fingerprint)
            , Http.header "voiceprint" (String.fromFloat voiceprint)
            ]
        , method = "GET"
        }


type Password
    = Password String


encodePassword : Password -> Json.Encode.Value
encodePassword password =
    case password of
        Password param1 ->
            Json.Encode.list identity [ Json.Encode.string param1 ]


decoderPassword : Json.Decode.Decoder Password
decoderPassword =
    Json.Decode.field "ctor" Json.Decode.string
        |> Json.Decode.andThen
            (\ctor ->
                Json.Decode.field "val" <|
                    case ctor of
                        "Password" ->
                            Json.Decode.string
                                |> Json.Decode.map Password

                        _ ->
                            Json.Decode.fail "Unexpected constructor"
            )


type Secret
    = Secret String


encodeSecret : Secret -> Json.Encode.Value
encodeSecret secret =
    case secret of
        Secret param1 ->
            Json.Encode.list identity [ Json.Encode.string param1 ]


decoderSecret : Json.Decode.Decoder Secret
decoderSecret =
    Json.Decode.field "ctor" Json.Decode.string
        |> Json.Decode.andThen
            (\ctor ->
                Json.Decode.field "val" <|
                    case ctor of
                        "Secret" ->
                            Json.Decode.string
                                |> Json.Decode.map Secret

                        _ ->
                            Json.Decode.fail "Unexpected constructor"
            )
