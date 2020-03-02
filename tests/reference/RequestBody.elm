module RequestBody exposing (..)

import Http
import Json.Decode
import Json.Encode


postFish : { body : Money } -> Cmd (Result Http.Error Fish)
postFish { body } =
    Http.request
        { tracker = Nothing
        , timeout = Nothing
        , expect = Http.expectJson identity decoderFish
        , body =
            body
                |> encodeMoney
                |> Http.jsonBody
        , url =
            String.concat
                [ "fish"
                , "?"
                , []
                    |> List.intersperse "&"
                    |> String.concat
                ]
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
            Json.Encode.list identity []

        Carp ->
            Json.Encode.list identity []

        Salmon ->
            Json.Encode.list identity []


decoderFish : Json.Decode.Decoder Fish
decoderFish =
    Json.Decode.field "ctor" Json.Decode.string
        |> Json.Decode.andThen
            (\ctor ->
                Json.Decode.field "val" <|
                    case ctor of
                        "Herring" ->
                            Json.Decode.succeed Herring

                        "Carp" ->
                            Json.Decode.succeed Carp

                        "Salmon" ->
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
                [ ( "amount", Json.Encode.int amount )
                , ( "currency", Json.Encode.string currency )
                ]


decoderMoney : Json.Decode.Decoder Money
decoderMoney =
    Json.Decode.field "ctor" Json.Decode.string
        |> Json.Decode.andThen
            (\ctor ->
                Json.Decode.field "val" <|
                    case ctor of
                        "Money" ->
                            Json.Decode.map2
                                (\amount currency ->
                                    { amount = amount, currency = currency }
                                )
                                Json.Decode.int
                                Json.Decode.string
                                |> Json.Decode.map Money

                        _ ->
                            Json.Decode.fail "Unexpected constructor"
            )
