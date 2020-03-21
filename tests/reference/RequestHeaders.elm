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
        , expect =
            Http.expectJson identity (Json.Decode.lazy (\_ -> decoderSecret))
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
encodePassword (Password param) =
    Json.Encode.string param


decoderPassword : Json.Decode.Decoder Password
decoderPassword =
    Json.Decode.string
        |> Json.Decode.map Password


type Secret
    = Secret String


encodeSecret : Secret -> Json.Encode.Value
encodeSecret (Secret param) =
    Json.Encode.string param


decoderSecret : Json.Decode.Decoder Secret
decoderSecret =
    Json.Decode.string
        |> Json.Decode.map Secret
