module Generated exposing (..)


getSocks : {} -> Cmd (Result Error Sock)
getSocks {} =
    Http.request
        { tracker = Nothing
        , timeout = Nothing
        , expect = Http.expectJson identity decoderSock
        , body = Http.emptyBody
        , url = String.concat
                    [ "http://example.com/"
                    , "socks"
                    , "?"
                    , [] |> List.intersperse "&" |> String.concat
                    ]
        , headers = []
        , method = "GET"
        }


type Sock
    = Sock { color : String, pattern : Pattern, holes : Int }


encodeSock : Sock -> Value
encodeSock sock =
    case sock of
        Sock { color, pattern, holes } ->
            Json.Encode.object
                [ ( "color", Json.Encode.string color )
                , ( "pattern", encodePattern pattern )
                , ( "holes", Json.Encode.int holes )
                ]


decoderSock : Decoder
decoderSock =
    Json.Decode.field "ctor" Json.Decode.string
        |> Json.Decode.andThen
               (\ctor ->
                    Json.Decode.field "val" <|
                        case ctor of
                            "Sock" ->
                                Json.Decode.map3
                                    (\color pattern holes ->
                                         { color = color
                                         , pattern = pattern
                                         , holes = holes
                                         }
                                    )
                                    Json.Decode.string
                                    decoderPattern
                                    Json.Decode.int
                                    |> Json.Decode.map Sock
                            _ ->
                                Json.Decode.fail "Unexpected constructor"
               )


type Pattern
    = None
    | Stripes
    | Dots
    | Other


encodePattern : Pattern -> Value
encodePattern pattern =
    case pattern of
        None ->
            Json.Encode.list identity []
        Stripes ->
            Json.Encode.list identity []
        Dots ->
            Json.Encode.list identity []
        Other ->
            Json.Encode.list identity []


decoderPattern : Decoder
decoderPattern =
    Json.Decode.field "ctor" Json.Decode.string
        |> Json.Decode.andThen
               (\ctor ->
                    Json.Decode.field "val" <|
                        case ctor of
                            "None" ->
                                Json.Decode.succeed None
                            "Stripes" ->
                                Json.Decode.succeed Stripes
                            "Dots" ->
                                Json.Decode.succeed Dots
                            "Other" ->
                                Json.Decode.succeed Other
                            _ ->
                                Json.Decode.fail "Unexpected constructor"
               )