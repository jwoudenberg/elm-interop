{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Servant.Interop.Elm.Generate (elmEncoder) where

import Servant.Interop.Elm.Values
import Data.Functor.Foldable (cata)
import Servant.Interop.Elm.Types (ElmTypeF'(..))

elmEncoder :: ElmType -> ElmValue (Any -> Value)
elmEncoder =
  cata $ \case
    Unit -> anyEncoder $ fn1 (var _always) (var _Json_Encode_null)
    Never -> anyEncoder $ var _never
    Bool -> anyEncoder $ var _Json_Encode_bool
    Int -> anyEncoder $ var _Json_Encode_int
    Float -> anyEncoder $ var _Json_Encode_float
    String -> anyEncoder $ var _Json_Encode_string
    List a -> anyEncoder $
      lambda "elems" $ \elems -> fn2 (var _Json_Encode_list) a elems
    Maybe f -> anyEncoder $ 
      lambda "x" $ \x ->
        mkCase x 
          [ matchCtor0 _Nothing (ctor0Encoder _Nothing)
          , matchCtor1 _Just "x" (\y -> ctor1Encoder _Just (fn1 f y))
          ]
    Result f g -> anyEncoder $ 
      lambda "x" $ \x ->
        mkCase x 
          [ matchCtor1 _Err "err" (\err -> ctor1Encoder _Err (fn1 f err))
          , matchCtor1 _Just "ok" (\ok -> ctor1Encoder _Just (fn1 g ok))
          ]
    Tuple2 _a _b -> undefined
    Tuple3 _a _b _c -> undefined
    Record _fields -> undefined
    Lambda _i _o -> undefined
    Defined _name -> undefined

data Any

anyEncoder :: ElmValue (a -> Value) -> ElmValue (Any -> Value)
anyEncoder = anyType

ctor0Encoder :: Variable a -> ElmValue Value
ctor0Encoder ctor =
       fn1
          (var _Json_Encode_object)
          (list
            [ tuple ("ctor") (fn1 (var _Json_Encode_string) (string (varName ctor)))
            , tuple ("value") (fn2 (var _Json_Encode_list) (var _identity) (list []))
            ]
          )

ctor1Encoder :: Variable (a -> b) -> ElmValue Value -> ElmValue Value
ctor1Encoder ctor param =
       fn1
          (var _Json_Encode_object)
          (list
            [ tuple ("ctor") (fn1 (var _Json_Encode_string) (string (varName ctor)))
            , tuple ("value") (fn2 (var _Json_Encode_list) (var _identity) (list [param]))
            ]
          )
