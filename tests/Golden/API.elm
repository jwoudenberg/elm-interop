type Foo
    = Foo
        { one : Result Int ( Int, String, String )
        , two : ()
        , three : Maybe (List String)
        , four : Maybe Bar
        }


type Bar
    = Bar Never Int Foo
    | Baz