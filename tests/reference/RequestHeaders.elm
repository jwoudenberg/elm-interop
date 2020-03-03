module RequestHeaders exposing (..)

import Http
import Json.Decode
import Json.Encode
import Url.Builder


getSecret :
    { password : Password, fingerprint : Int, voiceprint : Int }
    -> Cmd (Result Http.Error Secret)
getSecret { password, fingerprint, voiceprint } =
    Http.request
        { tracker = Nothing
        , timeout = Nothing
        , expect = Http.expectJson identity decoderSecret
        , body = Http.emptyBody
        , url = Url.Builder.absolute [ "secret" ] (List.concat [])
        , headers =
            [ Http.header "password" ((\(Password string) -> string) password)
            , Http.header "fingerprint" (String.fromInt fingerprint)
            , Http.header "voiceprint" (String.fromInt voiceprint)
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
