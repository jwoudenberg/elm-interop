getWish :
    {}
    -> ()
getWish =
    ()


type alias Unicorn =
    Never


encodeUnicorn :
    Unicorn
    -> Value
encodeUnicorn =
    never


decoderUnicorn :
    Decoder
decoderUnicorn =
    Json.Decode.fail "Cannot decode Never type from JSON"