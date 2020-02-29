getNameDogs : { name : Name } -> Cmd (Result Error Dog)
getNameDogs { name } =
    Http.request
        { tracker = Nothing
        , timeout = Nothing
        , expect = Http.expectJson identity decoderDog
        , body = Http.emptyBody
        , url = String.concat
                    [ "example.com/"
                    , String.join "/" [ "dogs", (\Name string -> string) name ]
                    , "?"
                    , [] |> List.intersperse "&" |> String.concat
                    ]
        , headers = []
        , method = "GET"
        }


getDogs : { minAge : Int } -> Cmd (Result Error Dog)
getDogs { minAge } =
    Http.request
        { tracker = Nothing
        , timeout = Nothing
        , expect = Http.expectJson identity decoderDog
        , body = Http.emptyBody
        , url = String.concat
                    [ "example.com/"
                    , "dogs"
                    , "?"
                    , [ String.concat [ "min-age=", fromInt minAge ] ]
                          |> List.intersperse "&"
                          |> String.concat
                    ]
        , headers = []
        , method = "GET"
        }


getToys : { fun : Bool } -> Cmd (Result Error Toy)
getToys { fun } =
    Http.request
        { tracker = Nothing
        , timeout = Nothing
        , expect = Http.expectJson identity decoderToy
        , body = Http.emptyBody
        , url = String.concat
                    [ "example.com/"
                    , "toys"
                    , "?"
                    , [ if fun then "fun" else "" ] |> List.intersperse "&"
                          |> String.concat
                    ]
        , headers = []
        , method = "GET"
        }


postToys : { body : Toy, authSmell : SmellRight } -> Cmd (Result Error ())
postToys { body, authSmell } =
    Http.request
        { tracker = Nothing
        , timeout = Nothing
        , expect = Http.expectJson identity (Json.Decode.succeed ())
        , body = body |> encodeToy |> Http.jsonBody
        , url = String.concat
                    [ "example.com/"
                    , "toys"
                    , "?"
                    , [] |> List.intersperse "&" |> String.concat
                    ]
        , headers = [ Http.header
                          "auth-smell"
                          ((\SmellRight bool -> if bool then "true" else "false"
                               )
                               authSmell
                          )
          ]
        , method = "POST"
        }


type Dog
    = Dog { name : Name, age : Int }


encodeDog : Dog -> Value
encodeDog dog =
    case dog of
        Dog { name, age } ->
            Json.Encode.object
                [ ( "name", encodeName name ), ( "age", Json.Encode.int age ) ]


decoderDog : Decoder
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


encodeName : Name -> Value
encodeName name =
    case name of
        Name param1 ->
            Json.Encode.list identity [ Json.Encode.string param1 ]


decoderName : Decoder
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


encodeToy : Toy -> Value
encodeToy toy =
    case toy of
        Bone ->
            Json.Encode.list identity []
        Ball ->
            Json.Encode.list identity []


decoderToy : Decoder
decoderToy =
    Json.Decode.field "ctor" Json.Decode.string
        |> Json.Decode.andThen
               (\ctor ->
                    Json.Decode.field "val" <|
                        case ctor of
                            "Bone" ->
                                Json.Decode.succeed Bone
                            "Ball" ->
                                Json.Decode.succeed Ball
                            _ ->
                                Json.Decode.fail "Unexpected constructor"
               )