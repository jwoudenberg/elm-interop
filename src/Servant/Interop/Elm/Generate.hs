{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Servant.Interop.Elm.Generate
  ( printEncoder,
    printDecoder,
  )
where

import Data.Bifunctor (first)
import qualified Data.Char as Char
import Data.Foldable (toList)
import Data.Functor.Foldable (Fix (Fix), cata)
import qualified Data.Text
import Servant.Interop.Elm.Types (ElmTypeDefinition (..), ElmTypeF' (..))
import Servant.Interop.Elm.Values
import qualified Text.PrettyPrint.Leijen.Text as PP
import qualified Wire

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
        mkCase
          x
          [ matchCtor0 _Nothing (ctor0Encoder _Nothing),
            matchCtor1 _Just "x" (\y -> ctor1Encoder _Just (fn1 f y))
          ]
    Result f g ->
      lambda $ matchVar "x" $ \x ->
        mkCase
          x
          [ matchCtor1 _Err "err" (\err -> ctor1Encoder _Err (fn1 f err)),
            matchCtor1 _Just "ok" (\ok -> ctor1Encoder _Just (fn1 g ok))
          ]
    Tuple2 f g ->
      lambda $ matchTuple2 "x" "y" $ \x y ->
        fn2 (var _Json_Encode_list) (var _identity) $
          list
            [ fn1 f x,
              fn1 g y
            ]
    Tuple3 f g h ->
      lambda $ matchTuple3 "x" "y" "z" $ \x y z ->
        fn2 (var _Json_Encode_list) (var _identity) $
          list
            [ fn1 f x,
              fn1 g y,
              fn1 h z
            ]
    Record fields ->
      lambda $
        matchRecordN
          (Wire.unFieldName . fst <$> fields)
          ( \field ->
              case lookup (Wire.FieldName (varName field)) fields of
                Nothing -> error "Lookup of record field failed"
                Just encoder ->
                  tuple (string (varName field)) (fn1 encoder (var field))
          )
          (fn1 (var _Json_Encode_object) . list)
    Lambda _ _ -> error "Cannot encode lambda function"
    Defined name -> v (decoderNameForType name)

encoderNameForType :: Wire.TypeName -> Data.Text.Text
encoderNameForType name =
  mconcat
    [ "encode",
      Wire.typeConstructor name
    ]

printEncoder :: Wire.TypeName -> ElmTypeDefinition -> PP.Doc
printEncoder name typeDef =
  printFunction
    (encoderNameForType name)
    (Fix (Lambda (Fix (Defined name)) (Fix (Defined valueTypeName))))
    (encoderForType name typeDef)

printDecoder :: Wire.TypeName -> ElmTypeDefinition -> PP.Doc
printDecoder name typeDef =
  printFunction
    (decoderNameForType name)
    (Fix (Defined (decoderTypeName name)))
    (decoderForType typeDef)

valueTypeName :: Wire.TypeName
valueTypeName =
  Wire.TypeName
    { Wire.typeConstructor = "Value",
      Wire.fromModule = "Json.Encode",
      Wire.parameters = []
    }

decoderTypeName :: Wire.TypeName -> Wire.TypeName
decoderTypeName decoded =
  Wire.TypeName
    { Wire.typeConstructor = "Decoder",
      Wire.fromModule = "Json.Decode",
      Wire.parameters = [Wire.typeConstructor decoded]
    }

encoderForType :: Wire.TypeName -> ElmTypeDefinition -> ElmValue (Any -> Value)
encoderForType typeName typeDef =
  case typeDef of
    Alias elmType -> elmEncoder elmType
    Custom constructors ->
      lambda $ matchVar lowerCasedTypeName $ \x ->
        mkCase x $ toList $ matchConstructor <$> constructors
      where
        lowerCasedTypeName =
          case Data.Text.uncons (Wire.typeConstructor typeName) of
            Nothing -> mempty
            Just (x, rest) -> Data.Text.cons (Char.toLower x) rest
        matchConstructor :: (Wire.ConstructorName, [ElmType]) -> (Pattern a0, ElmValue Value)
        matchConstructor (name, params) =
          case params of
            [Fix (Record fields)] ->
              matchCtorRecord
                (fromVarName $ Wire.unConstructorName name)
                (Wire.unFieldName . fst <$> fields)
                ( \field ->
                    case lookup (Wire.FieldName (varName field)) fields of
                      Nothing -> error "Lookup of record field failed"
                      Just elmType ->
                        tuple (string (varName field)) (fn1 (elmEncoder elmType) (var field))
                )
                (fn1 (var _Json_Encode_object) . list)
            _ ->
              matchCtorN
                (fromVarName $ Wire.unConstructorName name)
                (fst <$> keyedParams)
                ( \param ->
                    case lookup (varName param) keyedParams of
                      Nothing -> error "Lookup of constructor param failed"
                      Just elmType -> fn1 (elmEncoder elmType) (var param)
                )
                (fn2 (var _Json_Encode_list) (var _identity) . list)
              where
                keyedParams =
                  zipWith (\i param -> ("param" <> (Data.Text.pack $ show i), param)) [(1 :: Int) ..] params

data Any

anyEncoder :: ElmValue (a -> Value) -> ElmValue (Any -> Value)
anyEncoder = anyType

ctor0Encoder :: Variable a -> ElmValue Value
ctor0Encoder ctor =
  fn1
    (var _Json_Encode_object)
    ( list
        [ tuple ("ctor") (fn1 (var _Json_Encode_string) (string (varName ctor))),
          tuple ("val") (fn2 (var _Json_Encode_list) (var _identity) (list []))
        ]
    )

ctor1Encoder :: Variable (a -> b) -> ElmValue Value -> ElmValue Value
ctor1Encoder ctor param =
  fn1
    (var _Json_Encode_object)
    ( list
        [ tuple ("ctor") (fn1 (var _Json_Encode_string) (string (varName ctor))),
          tuple ("val") (fn2 (var _Json_Encode_list) (var _identity) (list [param]))
        ]
    )

elmDecoder :: ElmType -> ElmValue (Decoder Any)
elmDecoder =
  cata $ \case
    Unit -> anyDecoder $ fn1 (var _Json_Decode_succeed) unit
    Never -> fn1 (var _Json_Decode_fail) "Cannot decode Never type from JSON"
    Bool -> anyDecoder $ var _Json_Decode_bool
    Int -> anyDecoder $ var _Json_Decode_int
    Float -> anyDecoder $ var _Json_Decode_float
    String -> anyDecoder $ var _Json_Decode_string
    List a -> anyDecoder $ fn1 (var _Json_Decode_list) a
    Maybe f ->
      anyDecoder $
        fn2 (var _Json_Decode_field) "ctor" (var _Json_Decode_string)
          |> fn1
            (var _Json_Decode_andThen)
            ( lambda $ matchVar "ctor" $ \ctor ->
                mkCase
                  ctor
                  [ (matchString "Nothing", fn1 (var _Json_Decode_succeed) (var _Nothing)),
                    ( matchString "Just",
                      fn2 (var _Json_Decode_at) (list ["val", "0"]) f
                        |> fn1 (var _Json_Decode_map) (var _Just)
                    ),
                    matchVar "_" $ \_ -> fn1 (var _Json_Decode_fail) "Unexpected constructor"
                  ]
            )
    Result f g ->
      anyDecoder $
        fn2 (var _Json_Decode_field) "ctor" (var _Json_Decode_string)
          |> fn1
            (var _Json_Decode_andThen)
            ( lambda $ matchVar "x" $ \x ->
                mkCase
                  x
                  [ ( matchString "Err",
                      fn2 (var _Json_Decode_at) (list ["val", "0"]) f
                        |> fn1 (var _Json_Decode_map) (var _Err)
                    ),
                    ( matchString "Ok",
                      fn2 (var _Json_Decode_at) (list ["val", "0"]) g
                        |> fn1 (var _Json_Decode_map) (var _Ok)
                    ),
                    matchVar "_" $ \_ -> fn1 (var _Json_Decode_fail) "Unexpected constructor"
                  ]
            )
    Tuple2 f g -> anyDecoder $ fn3 (var _Json_Decode_map2) (var _Tuple_pair) f g
    Tuple3 f g h ->
      anyDecoder $ fn4 (var _Json_Decode_map3) triple f g h
      where
        triple :: ElmValue (a -> b -> c -> (a, b, c))
        triple =
          lambda $ matchVar "x" $ \x ->
            lambda $ matchVar "y" $ \y ->
              lambda $ matchVar "z" $ \z -> tuple3 x y z
    Record fields' -> decodeMapN (snd <$> fields) recordLambda
      where
        fields = first Wire.unFieldName <$> fields'
        recordLambda = recordLambda' (fst <$> reverse fields) emptyRecord
        recordLambda' :: [Data.Text.Text] -> Record -> ElmValue r
        recordLambda' [] record = mkRecord record
        recordLambda' (name : rest) record =
          anyType $ lambda $ matchVar name $ \x -> recordLambda' rest (addField name x record)
    Lambda _ _ -> error "Cannot decode lambda function from JSON"
    Defined name -> v (decoderNameForType name)

decodeMapN :: [ElmValue (Decoder g)] -> ElmValue (Decoder value) -> ElmValue (Decoder value)
decodeMapN decoders fn =
  case decoders of
    [] -> fn
    [d] -> fn2 (var _Json_Decode_map) (anyType fn) d
    [d1, d2] -> fn3 (var _Json_Decode_map2) (anyType fn) d1 d2
    [d1, d2, d3] -> fn4 (var _Json_Decode_map3) (anyType fn) d1 d2 d3
    [d1, d2, d3, d4] -> fn5 (var _Json_Decode_map4) (anyType fn) d1 d2 d3 d4
    [d1, d2, d3, d4, d5] -> fn6 (var _Json_Decode_map5) (anyType fn) d1 d2 d3 d4 d5
    [d1, d2, d3, d4, d5, d6] -> fn7 (var _Json_Decode_map6) (anyType fn) d1 d2 d3 d4 d5 d6
    [d1, d2, d3, d4, d5, d6, d7] -> fn8 (var _Json_Decode_map7) (anyType fn) d1 d2 d3 d4 d5 d6 d7
    d1 : d2 : d3 : d4 : d5 : d6 : d7 : d8 : dRest ->
      let first8 = fn9 (var _Json_Decode_map8) (anyType fn) d1 d2 d3 d4 d5 d6 d7 d8
       in decodeMapN dRest first8

anyDecoder :: ElmValue (Decoder a) -> ElmValue (Decoder Any)
anyDecoder = anyType

decoderNameForType :: Wire.TypeName -> Data.Text.Text
decoderNameForType name =
  mconcat
    [ "decoder",
      Wire.typeConstructor name
    ]

decoderForType :: ElmTypeDefinition -> ElmValue (Decoder Any)
decoderForType typeDef =
  case typeDef of
    Alias elmType -> elmDecoder elmType
    Custom constructors ->
      fn2 (var _Json_Decode_field) "ctor" (var _Json_Decode_string)
        |> fn1
          (var _Json_Decode_andThen)
          ( lambda $ matchVar "ctor" $ \ctor ->
              fn2 (var _Json_Decode_field) "val"
                $ mkCase ctor
                $ (toList $ decodeConstructor <$> constructors) <> [catchAll]
          )
      where
        catchAll = matchVar "_" $ \_ -> fn1 (var _Json_Decode_fail) "Unexpected constructor"
        decodeConstructor :: (Wire.ConstructorName, [ElmType]) -> (Pattern a, ElmValue (Decoder Any))
        decodeConstructor (Wire.ConstructorName ctorName, params) =
          ( matchString ctorName,
            decodeMapN (elmDecoder <$> params) (var (fromVarName ctorName))
          )
