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
encodeTurtle (Turtle param) =
    (\{ name, onBackOf } ->
        Json.Encode.object
            [ ( "name", Json.Encode.string name )
            , ( "onBackOf", encodeTurtle onBackOf )
            ]
    )
        param


decoderTurtle : Json.Decode.Decoder Turtle
decoderTurtle =
    Json.Decode.map2
        (\name onBackOf -> { name = name, onBackOf = onBackOf })
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "onBackOf" (Json.Decode.lazy (\_ -> decoderTurtle)))
        |> Json.Decode.map Turtle
