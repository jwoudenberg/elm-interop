type Turtle
    = Turtle { name : String, onBackOf : Turtle }


encodeTurtle :: Turtle -> Value
encodeTurtle turtle =
    case turtle of
        Turtle { name, onBackOf } ->
            Json.Encode.object
                [ ( "name", Json.Encode.string name )
                , ( "onBackOf", decoderTurtle onBackOf )
                ]


decoderTurtle :: Decoder
decoderTurtle =
    Json.Decode.field "ctor" Json.Decode.string
        |> Json.Decode.andThen
               (\ctor ->
                    Json.Decode.field "val" <|
                        case ctor of
                            "Turtle" ->
                                Json.Decode.map
                                    Turtle
                                    (Json.Decode.map2
                                         (\name onBackOf ->
                                              { name = name
                                              , onBackOf = onBackOf
                                              })
                                         Json.Decode.string
                                         decoderTurtle)
                            _ ->
                                Json.Decode.fail "Unexpected constructor")