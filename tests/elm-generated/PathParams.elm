module PathParams exposing (..)

import Http
import Json.Decode
import Json.Encode
import Url.Builder


getStopStartRoute :
    { start : City
    , stop : List City
    }
    -> Cmd (Result Http.Error Kilometers)
getStopStartRoute { start, stop } =
    Http.request
        { tracker = Nothing
        , timeout = Nothing
        , expect =
            Http.expectJson
                identity
                (Json.Decode.lazy (\_ -> decoderKilometers))
        , body = Http.emptyBody
        , url =
            Url.Builder.absolute
                [ "route"
                , (\(City string) -> string) start
                , List.map (\x -> (\(City string) -> string) x) stop
                    |> String.join "/"
                ]
                (List.concat [])
        , headers = []
        , method = "GET"
        }


type City
    = City String


encodeCity : City -> Json.Encode.Value
encodeCity city =
    case city of
        City param1 ->
            Json.Encode.object
                [ ( "ctor", Json.Encode.int 0 )
                , ( "val"
                  , Json.Encode.list identity [ Json.Encode.string param1 ]
                  )
                ]


decoderCity : Json.Decode.Decoder City
decoderCity =
    Json.Decode.field "ctor" Json.Decode.int
        |> Json.Decode.andThen
            (\ctor ->
                Json.Decode.field "val" <|
                    case ctor of
                        0 ->
                            Json.Decode.string
                                |> Json.Decode.index 0
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
            Json.Encode.object
                [ ( "ctor", Json.Encode.int 0 )
                , ( "val"
                  , Json.Encode.list identity [ Json.Encode.int param1 ]
                  )
                ]


decoderKilometers : Json.Decode.Decoder Kilometers
decoderKilometers =
    Json.Decode.field "ctor" Json.Decode.int
        |> Json.Decode.andThen
            (\ctor ->
                Json.Decode.field "val" <|
                    case ctor of
                        0 ->
                            Json.Decode.int
                                |> Json.Decode.index 0
                                |> Json.Decode.map Kilometers

                        _ ->
                            Json.Decode.fail "Unexpected constructor"
            )
