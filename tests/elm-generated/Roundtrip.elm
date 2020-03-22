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
    = Record
        { int : Int
        , text : String
        , list : List Bool
        , either : EitherIntBool
        }


encodeValue : Value -> Json.Encode.Value
encodeValue (Record param) =
    (\{ int, text, list, either } ->
        Json.Encode.object
            [ ( "int", Json.Encode.int int )
            , ( "text", Json.Encode.string text )
            , ( "list"
              , (\elems -> Json.Encode.list Json.Encode.bool elems) list
              )
            , ( "either", encodeEitherIntBool either )
            ]
    )
        param


decoderValue : Json.Decode.Decoder Value
decoderValue =
    Json.Decode.map4
        (\int text list either ->
            { int = int, text = text, list = list, either = either }
        )
        (Json.Decode.field "int" Json.Decode.int)
        (Json.Decode.field "text" Json.Decode.string)
        (Json.Decode.field "list" (Json.Decode.list Json.Decode.bool))
        (Json.Decode.field
            "either"
            (Json.Decode.lazy (\_ -> decoderEitherIntBool))
        )
        |> Json.Decode.map Record


type EitherIntBool
    = Left Int
    | Right Bool


encodeEitherIntBool : EitherIntBool -> Json.Encode.Value
encodeEitherIntBool eitherIntBool =
    case eitherIntBool of
        Left param ->
            Json.Encode.int param

        Right param ->
            Json.Encode.bool param


decoderEitherIntBool : Json.Decode.Decoder EitherIntBool
decoderEitherIntBool =
    Json.Decode.field "ctor" Json.Decode.string
        |> Json.Decode.andThen
            (\ctor ->
                Json.Decode.field "val" <|
                    case ctor of
                        "Left" ->
                            Json.Decode.int
                                |> Json.Decode.map Left

                        "Right" ->
                            Json.Decode.bool
                                |> Json.Decode.map Right

                        _ ->
                            Json.Decode.fail "Unexpected constructor"
            )
