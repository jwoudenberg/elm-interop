getWish : {} -> Cmd (Result Error Either)
getWish {} =
    Http.request
        { tracker = Nothing
        , timeout = Nothing
        , expect = Http.expectJson identity decoderEither
        , body = Http.emptyBody
        , url = String.concat
                    [ "example.com/"
                    , "wish"
                    , "?"
                    , [] |> List.intersperse "&" |> String.concat
                    ]
        , headers = []
        , method = "GET"
        }


type alias Unicorn =
    Never


encodeUnicorn : Unicorn -> Value
encodeUnicorn =
    never


decoderUnicorn : Decoder
decoderUnicorn =
    Json.Decode.fail "Cannot decode Never type from JSON"