type Fish
    = Herring
    | Carp
    | Salmon


encodeFish :: Fish -> Value
encodeFish fish =
    case fish of
        Herring ->
            Json.Encode.list identity []
        Carp ->
            Json.Encode.list identity []
        Salmon ->
            Json.Encode.list identity []


type Money
    = Money { amount : Int, currency : String }


encodeMoney :: Money -> Value
encodeMoney money =
    case money of
        Money { amount, currency } ->
            Json.Encode.object
                [ ( "amount", Json.Encode.int amount )
                , ( "currency", Json.Encode.string currency )
                ]