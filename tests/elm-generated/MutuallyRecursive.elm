module MutuallyRecursive exposing (..)

import Http
import Json.Decode
import Json.Encode
import Url.Builder


getDuet : {} -> Cmd (Result Http.Error BackAndForthText)
getDuet {} =
    Http.request
        { tracker = Nothing
        , timeout = Nothing
        , expect =
            Http.expectJson
                identity
                (Json.Decode.lazy (\_ -> decoderBackAndForthText))
        , body = Http.emptyBody
        , url = Url.Builder.absolute [ "duet" ] (List.concat [])
        , headers = []
        , method = "GET"
        }


type ForthText
    = Forth String BackAndForthText


encodeForthText : ForthText -> Json.Encode.Value
encodeForthText forthText =
    case forthText of
        Forth param1 param2 ->
            Json.Encode.object
                [ ( "ctor", Json.Encode.int 0 )
                , ( "val"
                  , Json.Encode.list
                        identity
                        [ Json.Encode.string param1
                        , encodeBackAndForthText param2
                        ]
                  )
                ]


decoderForthText : Json.Decode.Decoder ForthText
decoderForthText =
    Json.Decode.field "ctor" Json.Decode.int
        |> Json.Decode.andThen
            (\ctor ->
                Json.Decode.field "val" <|
                    case ctor of
                        0 ->
                            Json.Decode.map2
                                Forth
                                (Json.Decode.string
                                    |> Json.Decode.index 0
                                )
                                (Json.Decode.lazy
                                    (\_ -> decoderBackAndForthText)
                                    |> Json.Decode.index 1
                                )

                        _ ->
                            Json.Decode.fail "Unexpected constructor"
            )


type BackAndForthText
    = Back String ForthText


encodeBackAndForthText : BackAndForthText -> Json.Encode.Value
encodeBackAndForthText backAndForthText =
    case backAndForthText of
        Back param1 param2 ->
            Json.Encode.object
                [ ( "ctor", Json.Encode.int 0 )
                , ( "val"
                  , Json.Encode.list
                        identity
                        [ Json.Encode.string param1, encodeForthText param2 ]
                  )
                ]


decoderBackAndForthText : Json.Decode.Decoder BackAndForthText
decoderBackAndForthText =
    Json.Decode.field "ctor" Json.Decode.int
        |> Json.Decode.andThen
            (\ctor ->
                Json.Decode.field "val" <|
                    case ctor of
                        0 ->
                            Json.Decode.map2
                                Back
                                (Json.Decode.string
                                    |> Json.Decode.index 0
                                )
                                (Json.Decode.lazy (\_ -> decoderForthText)
                                    |> Json.Decode.index 1
                                )

                        _ ->
                            Json.Decode.fail "Unexpected constructor"
            )
