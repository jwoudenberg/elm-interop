module Generated exposing (..)

import Http
import Json.Decode
import Json.Encode


getWish : {} -> Cmd (Result Http.Error Either Void Unicorn)
getWish {} =
    Http.request
        { tracker = Nothing
        , timeout = Nothing
        , expect = Http.expectJson identity decoderEither
        , body = Http.emptyBody
        , url =
            String.concat
                [ "http://example.com/"
                , "wish"
                , "?"
                , [] |> List.intersperse "&" |> String.concat
                ]
        , headers = []
        , method = "GET"
        }


type alias Unicorn =
    Never


encodeUnicorn : Unicorn -> Json.Encode.Value
encodeUnicorn =
    never


decoderUnicorn : Json.Decode.Decoder Unicorn
decoderUnicorn =
    Json.Decode.fail "Cannot decode Never type from JSON"
