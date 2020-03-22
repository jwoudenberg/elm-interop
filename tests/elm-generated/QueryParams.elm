module QueryParams exposing (..)

import Http
import Json.Decode
import Json.Encode
import Url.Builder


getGroceries :
    { maxPrice : EuroCents
    , bio : Bool
    , brands : List String
    }
    -> Cmd (Result Http.Error (List Grocery))
getGroceries { maxPrice, bio, brands } =
    Http.request
        { tracker = Nothing
        , timeout = Nothing
        , expect =
            Http.expectJson
                identity
                (Json.Decode.list (Json.Decode.lazy (\_ -> decoderGrocery)))
        , body = Http.emptyBody
        , url =
            Url.Builder.absolute
                [ "groceries" ]
                (List.concat
                    [ [ (\(EuroCents int) -> Url.Builder.int "max-price" int)
                            maxPrice
                      ]
                    , [ Url.Builder.string
                            "bio"
                            (if bio then
                                "true"

                             else
                                "false"
                            )
                      ]
                    , List.map (\x -> Url.Builder.string "brands[]" x) brands
                    ]
                )
        , headers = []
        , method = "GET"
        }


type EuroCents
    = EuroCents Int


encodeEuroCents : EuroCents -> Json.Encode.Value
encodeEuroCents euroCents =
    case euroCents of
        EuroCents param1 ->
            Json.Encode.object
                [ ( "ctor", Json.Encode.int 0 )
                , ( "val"
                  , Json.Encode.list identity [ Json.Encode.int param1 ]
                  )
                ]


decoderEuroCents : Json.Decode.Decoder EuroCents
decoderEuroCents =
    Json.Decode.field "ctor" Json.Decode.int
        |> Json.Decode.andThen
            (\ctor ->
                Json.Decode.field "val" <|
                    case ctor of
                        0 ->
                            Json.Decode.int
                                |> Json.Decode.index 0
                                |> Json.Decode.map EuroCents

                        _ ->
                            Json.Decode.fail "Unexpected constructor"
            )


type Grocery
    = Courgette
    | Milk
    | PeanutButter
    | Chocolate


encodeGrocery : Grocery -> Json.Encode.Value
encodeGrocery grocery =
    case grocery of
        Courgette ->
            Json.Encode.object
                [ ( "ctor", Json.Encode.int 0 )
                , ( "val", Json.Encode.list identity [] )
                ]

        Milk ->
            Json.Encode.object
                [ ( "ctor", Json.Encode.int 1 )
                , ( "val", Json.Encode.list identity [] )
                ]

        PeanutButter ->
            Json.Encode.object
                [ ( "ctor", Json.Encode.int 2 )
                , ( "val", Json.Encode.list identity [] )
                ]

        Chocolate ->
            Json.Encode.object
                [ ( "ctor", Json.Encode.int 3 )
                , ( "val", Json.Encode.list identity [] )
                ]


decoderGrocery : Json.Decode.Decoder Grocery
decoderGrocery =
    Json.Decode.field "ctor" Json.Decode.int
        |> Json.Decode.andThen
            (\ctor ->
                Json.Decode.field "val" <|
                    case ctor of
                        0 ->
                            Json.Decode.succeed Courgette

                        1 ->
                            Json.Decode.succeed Milk

                        2 ->
                            Json.Decode.succeed PeanutButter

                        3 ->
                            Json.Decode.succeed Chocolate

                        _ ->
                            Json.Decode.fail "Unexpected constructor"
            )
