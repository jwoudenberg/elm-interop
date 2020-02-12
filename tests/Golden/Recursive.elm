type Turtle
    = Turtle { name : String, onBackOf : Turtle }


encodeTurtle :: Turtle -> Value
encodeTurtle turtle =
    case turtle of
        Turtle { name, onBackOf } ->
            Json.Encode.object
                [ ( "name", Json.Encode.string name )
                , ( "onBackOf", encodeTurtle onBackOf )
                ]