module PathParams exposing (..)

import Http
import Json.Decode
import Json.Encode


getStopStartRoute :
    { start : City, stop : List City }
    -> Cmd (Result Http.Error Kilometers)
getStopStartRoute { start, stop } =
    Http.request
        { tracker = Nothing
        , timeout = Nothing
        , expect = Http.expectJson identity decoderKilometers
        , body = Http.emptyBody
        , url =
            String.concat
                [ String.join
                    "/"
                    [ "route"
                    , (\(City string) -> string) start
                    , List.map (\x -> (\(City string) -> string) x) stop
                        |> String.join "/"
                    ]
                , "?"
                , []
                    |> List.intersperse "&"
                    |> String.concat
                ]
        , headers = []
        , method = "GET"
        }


type City
    = City String


encodeCity : City -> Json.Encode.Value
encodeCity city =
    case city of
        City param1 ->
            Json.Encode.list identity [ Json.Encode.string param1 ]


decoderCity : Json.Decode.Decoder City
decoderCity =
    Json.Decode.field "ctor" Json.Decode.string
        |> Json.Decode.andThen
            (\ctor ->
                Json.Decode.field "val" <|
                    case ctor of
                        "City" ->
                            Json.Decode.string
                                |> Json.Decode.map City

                        _ ->
                            Json.Decode.fail "Unexpected constructor"
            )


type Kilometers
    = Kilometers Int


encodeKilometers : Kilometers -> Json.Encode.Value
encodeKilometers kilometers =
    case kilometers of
        Kilometers param1 ->
            Json.Encode.list identity [ Json.Encode.int param1 ]


decoderKilometers : Json.Decode.Decoder Kilometers
decoderKilometers =
    Json.Decode.field "ctor" Json.Decode.string
        |> Json.Decode.andThen
            (\ctor ->
                Json.Decode.field "val" <|
                    case ctor of
                        "Kilometers" ->
                            Json.Decode.int
                                |> Json.Decode.map Kilometers

                        _ ->
                            Json.Decode.fail "Unexpected constructor"
            )
