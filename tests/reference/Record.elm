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
encodeSock (Sock param1) =
    Json.Encode.list
        identity
        [ (\{ color, pattern, holes } ->
            Json.Encode.object
                [ ( "color", Json.Encode.string color )
                , ( "pattern", encodePattern pattern )
                , ( "holes", Json.Encode.int holes )
                ]
          )
            param1
        ]


decoderSock : Json.Decode.Decoder Sock
decoderSock =
    Json.Decode.map3
        (\color pattern holes ->
            { color = color, pattern = pattern, holes = holes }
        )
        Json.Decode.string
        (Json.Decode.lazy (\_ -> decoderPattern))
        Json.Decode.int
        |> Json.Decode.index 0
        |> Json.Decode.map Sock


type Pattern
    = None
    | Stripes
    | Dots
    | Other


encodePattern : Pattern -> Json.Encode.Value
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


decoderPattern : Json.Decode.Decoder Pattern
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
