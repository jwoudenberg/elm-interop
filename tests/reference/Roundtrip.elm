module Roundtrip exposing (..)

import Http
import Json.Decode
import Json.Encode
import Url.Builder


getRoundtrip : {} -> Cmd (Result Http.Error Value)
getRoundtrip {} =
    Http.request
        { tracker = Nothing
        , timeout = Nothing
        , expect = Http.expectJson identity decoderValue
        , body = Http.emptyBody
        , url = Url.Builder.absolute [ "roundtrip" ] (List.concat [])
        , headers = []
        , method = "GET"
        }


postRoundtrip : { body : EitherTextValue } -> Cmd (Result Http.Error Value)
postRoundtrip { body } =
    Http.request
        { tracker = Nothing
        , timeout = Nothing
        , expect = Http.expectJson identity decoderValue
        , body =
            body
                |> encodeEitherTextValue
                |> Http.jsonBody
        , url = Url.Builder.absolute [ "roundtrip" ] (List.concat [])
        , headers = []
        , method = "POST"
        }


type EitherTextValue
    = Left String
    | Right Value


encodeEitherTextValue : EitherTextValue -> Json.Encode.Value
encodeEitherTextValue eitherTextValue =
    case eitherTextValue of
        Left param1 ->
            Json.Encode.list identity [ Json.Encode.string param1 ]

        Right param1 ->
            Json.Encode.list identity [ encodeValue param1 ]


decoderEitherTextValue : Json.Decode.Decoder EitherTextValue
decoderEitherTextValue =
    Json.Decode.field "ctor" Json.Decode.string
        |> Json.Decode.andThen
            (\ctor ->
                Json.Decode.field "val" <|
                    case ctor of
                        "Left" ->
                            Json.Decode.string
                                |> Json.Decode.map Left

                        "Right" ->
                            decoderValue
                                |> Json.Decode.map Right

                        _ ->
                            Json.Decode.fail "Unexpected constructor"
            )


type Value
    = Value Int


encodeValue : Value -> Json.Encode.Value
encodeValue value =
    case value of
        Value param1 ->
            Json.Encode.list identity [ Json.Encode.int param1 ]


decoderValue : Json.Decode.Decoder Value
decoderValue =
    Json.Decode.field "ctor" Json.Decode.string
        |> Json.Decode.andThen
            (\ctor ->
                Json.Decode.field "val" <|
                    case ctor of
                        "Value" ->
                            Json.Decode.int
                                |> Json.Decode.map Value

                        _ ->
                            Json.Decode.fail "Unexpected constructor"
            )
