{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Servant.Interop.Elm.Generate (elmEncoder) where

import qualified Wire
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
    List a -> lambda $ matchVar "elems" $ \elems -> fn2 (var _Json_Encode_list) a elems
    Maybe f -> 
      lambda $ matchVar "x" $ \x ->
        mkCase x 
          [ matchCtor0 _Nothing (ctor0Encoder _Nothing)
          , matchCtor1 _Just "x" (\y -> ctor1Encoder _Just (fn1 f y))
          ]
    Result f g ->
      lambda $ matchVar "x" $ \x ->
        mkCase x 
          [ matchCtor1 _Err "err" (\err -> ctor1Encoder _Err (fn1 f err))
          , matchCtor1 _Just "ok" (\ok -> ctor1Encoder _Just (fn1 g ok))
          ]
    Tuple2 f g -> 
      lambda $ matchTuple2 "x" "y" $ \x y ->
          fn2 (var _Json_Encode_list) (var _identity) $
            list
              [ fn1 f x
              , fn1 g y
              ]
    Tuple3 f g h -> 
      lambda $ matchTuple3 "x" "y" "z" $ \x y z ->
          fn2 (var _Json_Encode_list) (var _identity) $
            list
              [ fn1 f x
              , fn1 g y
              , fn1 h z
              ]
    Record fields -> 
      lambda $ matchRecordN
        (Wire.unFieldName . fst <$> fields)
        (\field -> 
            case lookup (Wire.FieldName (varName field)) fields of
              Nothing -> error "Lookup of record field failed"
              Just encoder ->
                tuple (string (varName field)) (fn1 encoder (var field))
        )
        (fn1 (var _Json_Encode_object) . list)
    Lambda _ _ -> error "Cannot encode lambda function"
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
