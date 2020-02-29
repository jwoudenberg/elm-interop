module Generated exposing (..)

import Http
import Json.Decode
import Json.Encode


getDuet : {} -> Cmd (Result Http.Error BackAndForth Text)
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


type Forth Text
    = Forth String BackAndForth Text


encodeForth : Forth Text -> Json.Encode.Value
encodeForth forth =
    case forth of
        (Forth param1 param2) ->
            Json.Encode.list
                identity
                [ Json.Encode.string param1, encodeBackAndForth param2 ]


decoderForth : Json.Decode.Decoder Forth
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


type BackAndForth Text
    = Back String Forth Text


encodeBackAndForth : BackAndForth Text -> Json.Encode.Value
encodeBackAndForth backAndForth =
    case backAndForth of
        (Back param1 param2) ->
            Json.Encode.list
                identity
                [ Json.Encode.string param1, encodeForth param2 ]


decoderBackAndForth : Json.Decode.Decoder BackAndForth
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