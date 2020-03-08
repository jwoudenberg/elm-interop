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


postRoundtrip : { body : Value } -> Cmd (Result Http.Error ())
postRoundtrip { body } =
    Http.request
        { tracker = Nothing
        , timeout = Nothing
        , expect = Http.expectJson identity (Json.Decode.succeed ())
        , body =
            body
                |> encodeValue
                |> Http.jsonBody
        , url = Url.Builder.absolute [ "roundtrip" ] (List.concat [])
        , headers = []
        , method = "POST"
        }


type Value
    = Value Int


encodeValue : Value -> Json.Encode.Value
encodeValue (Value param1) =
    Json.Encode.list identity [ Json.Encode.int param1 ]


decoderValue : Json.Decode.Decoder Value
decoderValue =
    Json.Decode.int
        |> Json.Decode.index 0
        |> Json.Decode.map Value
