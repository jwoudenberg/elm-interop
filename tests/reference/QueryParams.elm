module QueryParams exposing (..)

import Http
import Json.Decode
import Json.Encode


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
        , expect = Http.expectJson identity (Json.Decode.list decoderGrocery)
        , body = Http.emptyBody
        , url =
            String.concat
                [ "http://example.com/"
                , "groceries"
                , "?"
                , [ String.concat
                      [ "max-price="
                      , (\(EuroCents int) -> String.fromInt int) maxPrice
                      ]
                  , if bio then
                        "bio"

                    else
                        ""
                  , List.map (\x -> String.concat [ "brands[]=", x ]) brands
                      |> String.join "&"
                  ]
                    |> List.intersperse "&"
                    |> String.concat
                ]
        , headers = []
        , method = "GET"
        }


type EuroCents
    = EuroCents Int


encodeEuroCents : EuroCents -> Json.Encode.Value
encodeEuroCents euroCents =
    case euroCents of
        EuroCents param1 ->
            Json.Encode.list identity [ Json.Encode.int param1 ]


decoderEuroCents : Json.Decode.Decoder EuroCents
decoderEuroCents =
    Json.Decode.field "ctor" Json.Decode.string
        |> Json.Decode.andThen
            (\ctor ->
                Json.Decode.field "val" <|
                    case ctor of
                        "EuroCents" ->
                            Json.Decode.int
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
            Json.Encode.list identity []

        Milk ->
            Json.Encode.list identity []

        PeanutButter ->
            Json.Encode.list identity []

        Chocolate ->
            Json.Encode.list identity []


decoderGrocery : Json.Decode.Decoder Grocery
decoderGrocery =
    Json.Decode.field "ctor" Json.Decode.string
        |> Json.Decode.andThen
            (\ctor ->
                Json.Decode.field "val" <|
                    case ctor of
                        "Courgette" ->
                            Json.Decode.succeed Courgette

                        "Milk" ->
                            Json.Decode.succeed Milk

                        "PeanutButter" ->
                            Json.Decode.succeed PeanutButter

                        "Chocolate" ->
                            Json.Decode.succeed Chocolate

                        _ ->
                            Json.Decode.fail "Unexpected constructor"
            )
