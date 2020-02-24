getNameDogs :
    { name : Name
    }
    -> ()
getNameDogs =
    ()


getDogs :
    { minAge : Int
    }
    -> ()
getDogs =
    ()


getToys :
    { fun : Bool
    }
    -> ()
getToys =
    ()


postToys :
    { body : Toy
    , authSmell : String
    }
    -> ()
postToys =
    ()


type Dog
    = Dog { name : Name, age : Int }


encodeDog :
    Dog
    -> Value
encodeDog dog =
    case dog of
        Dog { name, age } ->
            Json.Encode.object
                [ ( "name", decoderName name ), ( "age", Json.Encode.int age ) ]


decoderDog :
    Decoder
decoderDog =
    Json.Decode.field "ctor" Json.Decode.string
        |> Json.Decode.andThen
               (\ctor ->
                    Json.Decode.field "val" <|
                        case ctor of
                            "Dog" ->
                                Json.Decode.map2
                                    (\name age -> { name = name, age = age })
                                    decoderName
                                    Json.Decode.int
                                    |> Json.Decode.map Dog
                            _ ->
                                Json.Decode.fail "Unexpected constructor"
               )


type Name
    = Name String


encodeName :
    Name
    -> Value
encodeName name =
    case name of
        Name param1 ->
            Json.Encode.list identity [ Json.Encode.string param1 ]


decoderName :
    Decoder
decoderName =
    Json.Decode.field "ctor" Json.Decode.string
        |> Json.Decode.andThen
               (\ctor ->
                    Json.Decode.field "val" <|
                        case ctor of
                            "Name" ->
                                Json.Decode.string |> Json.Decode.map Name
                            _ ->
                                Json.Decode.fail "Unexpected constructor"
               )


type Toy
    = Bone
    | Ball


encodeToy :
    Toy
    -> Value
encodeToy toy =
    case toy of
        Bone ->
            Json.Encode.list identity []
        Ball ->
            Json.Encode.list identity []


decoderToy :
    Decoder
decoderToy =
    Json.Decode.field "ctor" Json.Decode.string
        |> Json.Decode.andThen
               (\ctor ->
                    Json.Decode.field "val" <|
                        case ctor of
                            "Bone" ->
                                Bone
                            "Ball" ->
                                Ball
                            _ ->
                                Json.Decode.fail "Unexpected constructor"
               )