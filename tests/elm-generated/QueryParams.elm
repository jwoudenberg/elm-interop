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
encodeEuroCents (EuroCents param) =
    Json.Encode.int param


decoderEuroCents : Json.Decode.Decoder EuroCents
decoderEuroCents =
    Json.Decode.int
        |> Json.Decode.map EuroCents


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
