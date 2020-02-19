type Fish
    = Herring
    | Carp
    | Salmon


encodeFish : Fish -> Value
encodeFish fish =
    case fish of
        Herring ->
            Json.Encode.list identity []
        Carp ->
            Json.Encode.list identity []
        Salmon ->
            Json.Encode.list identity []


decoderFish : Decoder
decoderFish =
    Json.Decode.field "ctor" Json.Decode.string
        |> Json.Decode.andThen
               (\ctor ->
                    Json.Decode.field "val" <|
                        case ctor of
                            "Herring" ->
                                Herring
                            "Carp" ->
                                Carp
                            "Salmon" ->
                                Salmon
                            _ ->
                                Json.Decode.fail "Unexpected constructor"
               )


type Money
    = Money { amount : Int, currency : String }


encodeMoney : Money -> Value
encodeMoney money =
    case money of
        Money { amount, currency } ->
            Json.Encode.object
                [ ( "amount", Json.Encode.int amount )
                , ( "currency", Json.Encode.string currency )
                ]


decoderMoney : Decoder
decoderMoney =
    Json.Decode.field "ctor" Json.Decode.string
        |> Json.Decode.andThen
               (\ctor ->
                    Json.Decode.field "val" <|
                        case ctor of
                            "Money" ->
                                Json.Decode.map2
                                    (\amount currency ->
                                         { amount = amount
                                         , currency = currency
                                         }
                                    )
                                    Json.Decode.int
                                    Json.Decode.string
                                    |> Json.Decode.map Money
                            _ ->
                                Json.Decode.fail "Unexpected constructor"
               )