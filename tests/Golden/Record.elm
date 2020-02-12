type Sock
    = Sock { color : String, pattern : Pattern, holes : Int }


encodeSock :: Sock -> Value
encodeSock sock =
    case sock of
        Sock { color, pattern, holes } ->
            Json.Encode.object
                [ ( "color", Json.Encode.string color )
                , ( "pattern", encodePattern pattern )
                , ( "holes", Json.Encode.int holes )
                ]


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