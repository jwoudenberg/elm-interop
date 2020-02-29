module Generated exposing (..)


import Json.Decode
import Json.Encode
import Http


getTurtles : {} -> Cmd (Result Http.Error Turtle)
getTurtles {} =
    Http.request
        { tracker = Nothing
        , timeout = Nothing
        , expect = Http.expectJson identity decoderTurtle
        , body = Http.emptyBody
        , url = String.concat
                    [ "http://example.com/"
                    , "turtles"
                    , "?"
                    , [] |> List.intersperse "&" |> String.concat
                    ]
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
                [ ( "name", Json.Encode.string name )
                , ( "onBackOf", encodeTurtle onBackOf )
                ]


decoderTurtle : Json.Decode.Decoder
decoderTurtle =
    Json.Decode.field "ctor" Json.Decode.string
        |> Json.Decode.andThen
               (\ctor ->
                    Json.Decode.field "val" <|
                        case ctor of
                            "Turtle" ->
                                Json.Decode.map2
                                    (\name onBackOf ->
                                         { name = name
                                         , onBackOf = onBackOf
                                         }
                                    )
                                    Json.Decode.string
                                    decoderTurtle
                                    |> Json.Decode.map Turtle
                            _ ->
                                Json.Decode.fail "Unexpected constructor"
               )