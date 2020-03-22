module Record exposing (..)

import Http
import Json.Decode
import Json.Encode
import Url.Builder


getSocks : {} -> Cmd (Result Http.Error Sock)
getSocks {} =
    Http.request
        { tracker = Nothing
        , timeout = Nothing
        , expect =
            Http.expectJson identity (Json.Decode.lazy (\_ -> decoderSock))
        , body = Http.emptyBody
        , url = Url.Builder.absolute [ "socks" ] (List.concat [])
        , headers = []
        , method = "GET"
        }


type Sock
    = Sock { color : String, pattern : Pattern, holes : Int }


encodeSock : Sock -> Json.Encode.Value
encodeSock sock =
    case sock of
        Sock { color, pattern, holes } ->
            Json.Encode.object
                [ ( "ctor", Json.Encode.int 0 )
                , ( "val"
                  , Json.Encode.list
                        identity
                        [ Json.Encode.string color
                        , encodePattern pattern
                        , Json.Encode.int holes
                        ]
                  )
                ]


decoderSock : Json.Decode.Decoder Sock
decoderSock =
    Json.Decode.field "ctor" Json.Decode.int
        |> Json.Decode.andThen
            (\ctor ->
                Json.Decode.field "val" <|
                    case ctor of
                        0 ->
                            Json.Decode.map3
                                (\color pattern holes ->
                                    { color = color
                                    , pattern = pattern
                                    , holes = holes
                                    }
                                )
                                (Json.Decode.field "0" Json.Decode.string)
                                (Json.Decode.field
                                    "1"
                                    (Json.Decode.lazy (\_ -> decoderPattern))
                                )
                                (Json.Decode.field "2" Json.Decode.int)
                                |> Json.Decode.map Sock

                        _ ->
                            Json.Decode.fail "Unexpected constructor"
            )


type Pattern
    = None
    | Stripes
    | Dots
    | Other


encodePattern : Pattern -> Json.Encode.Value
encodePattern pattern =
    case pattern of
        None ->
            Json.Encode.object
                [ ( "ctor", Json.Encode.int 0 )
                , ( "val", Json.Encode.list identity [] )
                ]

        Stripes ->
            Json.Encode.object
                [ ( "ctor", Json.Encode.int 1 )
                , ( "val", Json.Encode.list identity [] )
                ]

        Dots ->
            Json.Encode.object
                [ ( "ctor", Json.Encode.int 2 )
                , ( "val", Json.Encode.list identity [] )
                ]

        Other ->
            Json.Encode.object
                [ ( "ctor", Json.Encode.int 3 )
                , ( "val", Json.Encode.list identity [] )
                ]


decoderPattern : Json.Decode.Decoder Pattern
decoderPattern =
    Json.Decode.field "ctor" Json.Decode.int
        |> Json.Decode.andThen
            (\ctor ->
                Json.Decode.field "val" <|
                    case ctor of
                        0 ->
                            Json.Decode.succeed None

                        1 ->
                            Json.Decode.succeed Stripes

                        2 ->
                            Json.Decode.succeed Dots

                        3 ->
                            Json.Decode.succeed Other

                        _ ->
                            Json.Decode.fail "Unexpected constructor"
            )
