module Void exposing (..)

import Http
import Json.Decode
import Json.Encode
import Url.Builder


getWish : {} -> Cmd (Result Http.Error (Result Never Unicorn))
getWish {} =
    Http.request
        { tracker = Nothing
        , timeout = Nothing
        , expect =
            Http.expectJson
                identity
                (Json.Decode.field "ctor" Json.Decode.int
                    |> Json.Decode.andThen
                        (\x ->
                            case x of
                                0 ->
                                    Json.Decode.at
                                        [ "val", "0" ]
                                        (Json.Decode.fail
                                            "Cannot decode Never type from JSON"
                                        )
                                        |> Json.Decode.map Err

                                1 ->
                                    Json.Decode.at
                                        [ "val", "0" ]
                                        (Json.Decode.lazy (\_ -> decoderUnicorn)
                                        )
                                        |> Json.Decode.map Ok

                                _ ->
                                    Json.Decode.fail "Unexpected constructor"
                        )
                )
        , body = Http.emptyBody
        , url = Url.Builder.absolute [ "wish" ] (List.concat [])
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
