module RequestBody exposing (..)

import Http
import Json.Decode
import Json.Encode
import Url.Builder


postFish : { body : Money } -> Cmd (Result Http.Error Fish)
postFish { body } =
    Http.request
        { tracker = Nothing
        , timeout = Nothing
        , expect =
            Http.expectJson identity (Json.Decode.lazy (\_ -> decoderFish))
        , body =
            body
                |> encodeMoney
                |> Http.jsonBody
        , url = Url.Builder.absolute [ "fish" ] (List.concat [])
        , headers = []
        , method = "POST"
        }


type Fish
    = Herring
    | Carp
    | Salmon


encodeFish : Fish -> Json.Encode.Value
encodeFish fish =
    case fish of
        Herring ->
            Json.Encode.object
                [ ( "ctor", Json.Encode.int 0 )
                , ( "val", Json.Encode.list identity [] )
                ]

        Carp ->
            Json.Encode.object
                [ ( "ctor", Json.Encode.int 1 )
                , ( "val", Json.Encode.list identity [] )
                ]

        Salmon ->
            Json.Encode.object
                [ ( "ctor", Json.Encode.int 2 )
                , ( "val", Json.Encode.list identity [] )
                ]


decoderFish : Json.Decode.Decoder Fish
decoderFish =
    Json.Decode.field "ctor" Json.Decode.int
        |> Json.Decode.andThen
            (\ctor ->
                Json.Decode.field "val" <|
                    case ctor of
                        0 ->
                            Json.Decode.succeed Herring

                        1 ->
                            Json.Decode.succeed Carp

                        2 ->
                            Json.Decode.succeed Salmon

                        _ ->
                            Json.Decode.fail "Unexpected constructor"
            )


type Money
    = Money { amount : Int, currency : String }


encodeMoney : Money -> Json.Encode.Value
encodeMoney money =
    case money of
        Money { amount, currency } ->
            Json.Encode.object
                [ ( "ctor", Json.Encode.int 0 )
                , ( "val"
                  , Json.Encode.list
                        identity
                        [ Json.Encode.int amount, Json.Encode.string currency ]
                  )
                ]


decoderMoney : Json.Decode.Decoder Money
decoderMoney =
    Json.Decode.field "ctor" Json.Decode.int
        |> Json.Decode.andThen
            (\ctor ->
                Json.Decode.field "val" <|
                    case ctor of
                        0 ->
                            Json.Decode.map2
                                (\amount currency ->
                                    { amount = amount, currency = currency }
                                )
                                (Json.Decode.field "0" Json.Decode.int)
                                (Json.Decode.field "1" Json.Decode.string)
                                |> Json.Decode.map Money

                        _ ->
                            Json.Decode.fail "Unexpected constructor"
            )
