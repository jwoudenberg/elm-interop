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
        , expect = Http.expectJson identity (Json.Decode.list decoderDog)
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
        , expect = Http.expectJson identity decoderToy
        , body = Http.emptyBody
        , url = Url.Builder.absolute [ "toys" ] (List.concat [])
        , headers = []
        , method = "GET"
        }


type Dog
    = Dog { name : Name, age : Int }


encodeDog : Dog -> Json.Encode.Value
encodeDog (Dog param1) =
    Json.Encode.list
        identity
        [ (\{ name, age } ->
                Json.Encode.object
                    [ ( "name", encodeName name )
                    , ( "age", Json.Encode.int age )
                    ]
            )
            param1
        ]


decoderDog : Json.Decode.Decoder Dog
decoderDog =
    Json.Decode.map2
        (\name age -> { name = name, age = age })
        decoderName
        Json.Decode.int
        |> Json.Decode.index 0
        |> Json.Decode.map Dog


type Name
    = Name String


encodeName : Name -> Json.Encode.Value
encodeName (Name param1) =
    Json.Encode.list identity [ Json.Encode.string param1 ]


decoderName : Json.Decode.Decoder Name
decoderName =
    Json.Decode.string
        |> Json.Decode.index 0
        |> Json.Decode.map Name


type Toy
    = Bone
    | Ball


encodeToy : Toy -> Json.Encode.Value
encodeToy toy =
    case toy of
        Bone ->
            Json.Encode.list identity []

        Ball ->
            Json.Encode.list identity []


decoderToy : Json.Decode.Decoder Toy
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
