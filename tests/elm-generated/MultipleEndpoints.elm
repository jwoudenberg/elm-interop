module MultipleEndpoints exposing (..)

import Http
import Json.Decode
import Json.Encode
import Url.Builder


getDogs : {} -> Cmd (Result Http.Error (List Dog))
getDogs {} =
    Http.request
        { tracker = Nothing
        , timeout = Nothing
        , expect =
            Http.expectJson
                identity
                (Json.Decode.list (Json.Decode.lazy (\_ -> decoderDog)))
        , body = Http.emptyBody
        , url = Url.Builder.absolute [ "dogs" ] (List.concat [])
        , headers = []
        , method = "GET"
        }


getToys : {} -> Cmd (Result Http.Error Toy)
getToys {} =
    Http.request
        { tracker = Nothing
        , timeout = Nothing
        , expect =
            Http.expectJson identity (Json.Decode.lazy (\_ -> decoderToy))
        , body = Http.emptyBody
        , url = Url.Builder.absolute [ "toys" ] (List.concat [])
        , headers = []
        , method = "GET"
        }


type Dog
    = Dog { name : Name, age : Int }


encodeDog : Dog -> Json.Encode.Value
encodeDog dog =
    case dog of
        Dog { name, age } ->
            Json.Encode.object
                [ ( "ctor", Json.Encode.int 0 )
                , ( "val"
                  , Json.Encode.list
                        identity
                        [ encodeName name, Json.Encode.int age ]
                  )
                ]


decoderDog : Json.Decode.Decoder Dog
decoderDog =
    Json.Decode.field "ctor" Json.Decode.int
        |> Json.Decode.andThen
            (\ctor ->
                Json.Decode.field "val" <|
                    case ctor of
                        0 ->
                            Json.Decode.map2
                                (\name age -> { name = name, age = age })
                                (Json.Decode.field
                                    "0"
                                    (Json.Decode.lazy (\_ -> decoderName))
                                )
                                (Json.Decode.field "1" Json.Decode.int)
                                |> Json.Decode.map Dog

                        _ ->
                            Json.Decode.fail "Unexpected constructor"
            )


type Name
    = Name String


encodeName : Name -> Json.Encode.Value
encodeName name =
    case name of
        Name param1 ->
            Json.Encode.object
                [ ( "ctor", Json.Encode.int 0 )
                , ( "val"
                  , Json.Encode.list identity [ Json.Encode.string param1 ]
                  )
                ]


decoderName : Json.Decode.Decoder Name
decoderName =
    Json.Decode.field "ctor" Json.Decode.int
        |> Json.Decode.andThen
            (\ctor ->
                Json.Decode.field "val" <|
                    case ctor of
                        0 ->
                            Json.Decode.string
                                |> Json.Decode.index 0
                                |> Json.Decode.map Name

                        _ ->
                            Json.Decode.fail "Unexpected constructor"
            )


type Toy
    = Bone
    | Ball


encodeToy : Toy -> Json.Encode.Value
encodeToy toy =
    case toy of
        Bone ->
            Json.Encode.object
                [ ( "ctor", Json.Encode.int 0 )
                , ( "val", Json.Encode.list identity [] )
                ]

        Ball ->
            Json.Encode.object
                [ ( "ctor", Json.Encode.int 1 )
                , ( "val", Json.Encode.list identity [] )
                ]


decoderToy : Json.Decode.Decoder Toy
decoderToy =
    Json.Decode.field "ctor" Json.Decode.int
        |> Json.Decode.andThen
            (\ctor ->
                Json.Decode.field "val" <|
                    case ctor of
                        0 ->
                            Json.Decode.succeed Bone

                        1 ->
                            Json.Decode.succeed Ball

                        _ ->
                            Json.Decode.fail "Unexpected constructor"
            )
