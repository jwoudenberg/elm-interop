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
        , expect =
            Http.expectJson identity (Json.Decode.lazy (\_ -> decoderValue))
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
    = Record { int : Int, text : String }


encodeValue : Value -> Json.Encode.Value
encodeValue (Record param) =
    (\{ int, text } ->
        Json.Encode.object
            [ ( "int", Json.Encode.int int )
            , ( "text", Json.Encode.string text )
            ]
    )
        param


decoderValue : Json.Decode.Decoder Value
decoderValue =
    Json.Decode.map2
        (\int text -> { int = int, text = text })
        (Json.Decode.field "int" Json.Decode.int)
        (Json.Decode.field "text" Json.Decode.string)
        |> Json.Decode.map Record
