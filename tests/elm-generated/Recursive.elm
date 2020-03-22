module Recursive exposing (..)

import Http
import Json.Decode
import Json.Encode
import Url.Builder


getTurtles : {} -> Cmd (Result Http.Error Turtle)
getTurtles {} =
    Http.request
        { tracker = Nothing
        , timeout = Nothing
        , expect =
            Http.expectJson identity (Json.Decode.lazy (\_ -> decoderTurtle))
        , body = Http.emptyBody
        , url = Url.Builder.absolute [ "turtles" ] (List.concat [])
        , headers = []
        , method = "GET"
        }


type Turtle
    = Turtle { name : String, onBackOf : Turtle }


encodeTurtle : Turtle -> Json.Encode.Value
encodeTurtle turtle =
    case turtle of
        Turtle { name, onBackOf } ->
            Json.Encode.object
                [ ( "ctor", Json.Encode.int 0 )
                , ( "val"
                  , Json.Encode.list
                        identity
                        [ Json.Encode.string name, encodeTurtle onBackOf ]
                  )
                ]


decoderTurtle : Json.Decode.Decoder Turtle
decoderTurtle =
    Json.Decode.field "ctor" Json.Decode.int
        |> Json.Decode.andThen
            (\ctor ->
                Json.Decode.field "val" <|
                    case ctor of
                        0 ->
                            Json.Decode.map2
                                (\name onBackOf ->
                                    { name = name, onBackOf = onBackOf }
                                )
                                (Json.Decode.field "0" Json.Decode.string)
                                (Json.Decode.field
                                    "1"
                                    (Json.Decode.lazy (\_ -> decoderTurtle))
                                )
                                |> Json.Decode.map Turtle

                        _ ->
                            Json.Decode.fail "Unexpected constructor"
            )
