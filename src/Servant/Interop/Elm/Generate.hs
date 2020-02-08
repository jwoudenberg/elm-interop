{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Servant.Interop.Elm.Generate (elmEncoder) where

import Servant.Interop.Elm.Values
import Data.Functor.Foldable (Fix(Fix), cata)
import Data.String (IsString(fromString))
import Data.Text (Text)
import Servant.Interop.Elm.Types (ElmTypeF'(..))

elmEncoder :: ElmType -> ElmValue a
elmEncoder =
  cata $ \case
    Unit -> fn1 "Basics.always" unit
    Never -> "Basics.never"
    Bool -> "Json.Encode.bool"
    Int -> "Json.Encode.int"
    Float -> "Json.Encode.float"
    String -> "Json.Encode.string"
    List a ->
      lambda "list" $ \list ->
        fn2 "List.map" a list |> fn1 "Json.Encode.list" a
    Maybe a -> customTypeEncoder [("Nothing", []), ("Just", [a])]
    Result err ok -> customTypeEncoder [("Err", [err]), ("Ok", [ok])]
    Tuple2 _a _b -> undefined
    Tuple3 _a _b _c -> undefined
    Record _fields -> undefined
    Lambda _i _o -> undefined
    Defined _name _ -> undefined

customTypeEncoder :: [(Text, [ElmValue x])] -> ElmValue y
customTypeEncoder ctors =
  lambda ("x") $ \x ->
    mkCase x (uncurry constructorEncoder <$> ctors)

constructorEncoder :: Text -> [ElmValue x] -> (Pattern z, ElmValue y)
constructorEncoder name paramEncoders =
  let vars =
        take (length paramEncoders) $ (("x" <>) . show) <$> ([1 ..] :: [Int])
   in ( Fix (ConstructorPat name (p0 . fromString <$> vars))
      , fn1
          ("Json.Encode.object")
          (l [ tuple ("ctor") (fn1 ("Json.Encode.string") (v name))
          , tuple ("value")
            ( l $
              zipWith
                (\param encoder -> encoder <| fromString param)
                vars
                paramEncoders)
          ]))
