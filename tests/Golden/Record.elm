type Sock
    = Sock { color : String, pattern : Pattern, holes : Int }


encodeSock :: Sock -> Value
encodeSock sock =
    case sock of
        Sock { color, pattern, holes } ->
            Json.Encode.object
                [ ( "color", Json.Encode.string color )
                , ( "pattern", decoderPattern pattern )
                , ( "holes", Json.Encode.int holes )
                ]


decoderSock :: Decoder
decoderSock =
    Json.Decode.field "ctor" Json.Decode.string
        |> Json.Decode.andThen
               (\ctor ->
                    Json.Decode.field "val" <|
                        case ctor of
                            "Sock" ->
                                Json.Decode.map
                                    Sock
                                    (Json.Decode.map3
                                         (\color pattern holes ->
                                              { color = color
                                              , pattern = pattern
                                              , holes = holes
                                              })
                                         Json.Decode.string
                                         decoderPattern
                                         Json.Decode.int)
                            _ ->
                                Json.Decode.fail "Unexpected constructor")


type Pattern
    = None
    | Stripes
    | Dots
    | Other


encodePattern :: Pattern -> Value
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


decoderPattern :: Decoder
decoderPattern =
    Json.Decode.field "ctor" Json.Decode.string
        |> Json.Decode.andThen
               (\ctor ->
                    Json.Decode.field "val" <|
                        case ctor of
                            "None" ->
                                None
                            "Stripes" ->
                                Stripes
                            "Dots" ->
                                Dots
                            "Other" ->
                                Other
                            _ ->
                                Json.Decode.fail "Unexpected constructor")