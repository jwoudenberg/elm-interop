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
        , either : Result Int Bool
        }


encodeValue : Value -> Json.Encode.Value
encodeValue value =
    case value of
        Record { int, text, list, either } ->
            Json.Encode.object
                [ ( "ctor", Json.Encode.int 0 )
                , ( "val"
                  , Json.Encode.list
                        identity
                        [ Json.Encode.int int
                        , Json.Encode.string text
                        , (\elems -> Json.Encode.list Json.Encode.bool elems)
                            list
                        , (\x ->
                            case x of
                                Err err ->
                                    Json.Encode.object
                                        [ ( "ctor", Json.Encode.int 0 )
                                        , ( "val"
                                          , Json.Encode.list
                                                identity
                                                [ Json.Encode.int err ]
                                          )
                                        ]

                                Ok ok ->
                                    Json.Encode.object
                                        [ ( "ctor", Json.Encode.int 1 )
                                        , ( "val"
                                          , Json.Encode.list
                                                identity
                                                [ Json.Encode.bool ok ]
                                          )
                                        ]
                          )
                            either
                        ]
                  )
                ]


decoderValue : Json.Decode.Decoder Value
decoderValue =
    Json.Decode.field "ctor" Json.Decode.int
        |> Json.Decode.andThen
            (\ctor ->
                Json.Decode.field "val" <|
                    case ctor of
                        0 ->
                            Json.Decode.map4
                                (\int text list either ->
                                    { int = int
                                    , text = text
                                    , list = list
                                    , either = either
                                    }
                                )
                                (Json.Decode.field "0" Json.Decode.int)
                                (Json.Decode.field "1" Json.Decode.string)
                                (Json.Decode.field
                                    "2"
                                    (Json.Decode.list Json.Decode.bool)
                                )
                                (Json.Decode.field
                                    "3"
                                    (Json.Decode.field "ctor" Json.Decode.int
                                        |> Json.Decode.andThen
                                            (\x ->
                                                case x of
                                                    0 ->
                                                        Json.Decode.at
                                                            [ "val", "0" ]
                                                            Json.Decode.int
                                                            |> Json.Decode.map
                                                                Err

                                                    1 ->
                                                        Json.Decode.at
                                                            [ "val", "0" ]
                                                            Json.Decode.bool
                                                            |> Json.Decode.map
                                                                Ok

                                                    _ ->
                                                        Json.Decode.fail
                                                            "Unexpected constructor"
                                            )
                                    )
                                )
                                |> Json.Decode.map Record

                        _ ->
                            Json.Decode.fail "Unexpected constructor"
            )
