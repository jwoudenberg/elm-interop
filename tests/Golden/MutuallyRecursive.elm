module Generated exposing (..)


getDuet : {} -> Cmd (Result Error BackAndForth)
getDuet {} =
    Http.request
        { tracker = Nothing
        , timeout = Nothing
        , expect = Http.expectJson identity decoderBackAndForth
        , body = Http.emptyBody
        , url = String.concat
                    [ "http://example.com/"
                    , "duet"
                    , "?"
                    , [] |> List.intersperse "&" |> String.concat
                    ]
        , headers = []
        , method = "GET"
        }


type Forth
    = Forth String BackAndForth


encodeForth : Forth -> Value
encodeForth forth =
    case forth of
        Forth param1 param2 ->
            Json.Encode.list
                identity
                [ Json.Encode.string param1, encodeBackAndForth param2 ]


decoderForth : Decoder
decoderForth =
    Json.Decode.field "ctor" Json.Decode.string
        |> Json.Decode.andThen
               (\ctor ->
                    Json.Decode.field "val" <|
                        case ctor of
                            "Forth" ->
                                Json.Decode.map2
                                    Forth
                                    Json.Decode.string
                                    decoderBackAndForth
                            _ ->
                                Json.Decode.fail "Unexpected constructor"
               )


type BackAndForth
    = Back String Forth


encodeBackAndForth : BackAndForth -> Value
encodeBackAndForth backAndForth =
    case backAndForth of
        Back param1 param2 ->
            Json.Encode.list
                identity
                [ Json.Encode.string param1, encodeForth param2 ]


decoderBackAndForth : Decoder
decoderBackAndForth =
    Json.Decode.field "ctor" Json.Decode.string
        |> Json.Decode.andThen
               (\ctor ->
                    Json.Decode.field "val" <|
                        case ctor of
                            "Back" ->
                                Json.Decode.map2
                                    Back
                                    Json.Decode.string
                                    decoderForth
                            _ ->
                                Json.Decode.fail "Unexpected constructor"
               )