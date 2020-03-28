module MutuallyRecursive exposing (..)

import Http
import Json.Decode
import Json.Encode
import Url.Builder


getDuet : {} -> Cmd (Result Http.Error BackAndForth)
getDuet {} =
    Http.request
        { tracker = Nothing
        , timeout = Nothing
        , expect =
            Http.expectJson
                identity
                (Json.Decode.lazy (\_ -> decoderBackAndForth))
        , body = Http.emptyBody
        , url = Url.Builder.absolute [ "duet" ] (List.concat [])
        , headers = []
        , method = "GET"
        }


type Forth
    = Forth String BackAndForth


encodeForth : Forth -> Json.Encode.Value
encodeForth forth =
    case forth of
        Forth param1 param2 ->
            Json.Encode.object
                [ ( "ctor", Json.Encode.int 0 )
                , ( "val"
                  , Json.Encode.list
                        identity
                        [ Json.Encode.string param1, encodeBackAndForth param2 ]
                  )
                ]


decoderForth : Json.Decode.Decoder Forth
decoderForth =
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
                                (Json.Decode.lazy (\_ -> decoderBackAndForth)
                                    |> Json.Decode.index 1
                                )

                        _ ->
                            Json.Decode.fail "Unexpected constructor"
            )


type BackAndForth
    = Back String Forth


encodeBackAndForth : BackAndForth -> Json.Encode.Value
encodeBackAndForth backAndForth =
    case backAndForth of
        Back param1 param2 ->
            Json.Encode.object
                [ ( "ctor", Json.Encode.int 0 )
                , ( "val"
                  , Json.Encode.list
                        identity
                        [ Json.Encode.string param1, encodeForth param2 ]
                  )
                ]


decoderBackAndForth : Json.Decode.Decoder BackAndForth
decoderBackAndForth =
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
                                (Json.Decode.lazy (\_ -> decoderForth)
                                    |> Json.Decode.index 1
                                )

                        _ ->
                            Json.Decode.fail "Unexpected constructor"
            )
