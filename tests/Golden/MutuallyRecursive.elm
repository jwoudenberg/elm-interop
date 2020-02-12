type Forth
    = Forth String BackAndForth


encodeForth :: Forth -> Value
encodeForth forth =
    case forth of
        Forth param1 param2 ->
            Json.Encode.list
                identity
                [ Json.Encode.string param1, encodeBackAndForth param2 ]


type BackAndForth
    = Back String Forth


encodeBackAndForth :: BackAndForth -> Value
encodeBackAndForth backAndForth =
    case backAndForth of
        Back param1 param2 ->
            Json.Encode.list
                identity
                [ Json.Encode.string param1, encodeForth param2 ]