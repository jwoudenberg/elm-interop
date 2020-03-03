{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Servant.Interop.Elm.Generate
  ( generateEncoder,
    generateDecoder,
    generateClient,
  )
where

import Data.Bifunctor (first)
import qualified Data.Char as Char
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Functor.Foldable (Fix (Fix), cata)
import Data.Maybe (maybeToList)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Servant.Interop (Endpoint (..), Path (..), QueryVal (..))
import Servant.Interop.Elm.Types (ElmTypeDefinition (..), ElmTypeF (..), TypeName (..), fromWireType, toElmTypeName)
import Servant.Interop.Elm.Values
import qualified Wire
import qualified Wire.Parameter as Parameter

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
          (fn1 (var _Json_Encode_object) . list . fmap snd)
    Cmd _ -> error "Cannot encode Cmd"
    Lambda _ _ -> error "Cannot encode lambda function"
    Defined name _ -> v (encoderNameForType name)

encoderNameForType :: TypeName -> T.Text
encoderNameForType (TypeName name) =
  "encode" <> name

generateEncoder :: TypeName -> ElmTypeDefinition -> ElmFunction
generateEncoder name typeDef =
  ElmFunction
    { fnName = encoderNameForType name,
      fnType = Fix (Lambda (Fix (Defined name [])) (Fix (Defined _Json_Encode_Value []))),
      fnImplementation = encoderForType name typeDef
    }

generateDecoder :: TypeName -> ElmTypeDefinition -> ElmFunction
generateDecoder name typeDef =
  ElmFunction
    { fnName = decoderNameForType name,
      fnType = Fix (Defined _Json_Decode_Decoder [Fix (Defined name [])]),
      fnImplementation = decoderForType typeDef
    }

_Json_Encode_Value :: TypeName
_Json_Encode_Value = "Json.Encode.Value"

_Json_Decode_Decoder :: TypeName
_Json_Decode_Decoder = "Json.Decode.Decoder"

encoderForType :: TypeName -> ElmTypeDefinition -> ElmValue (Any -> Value)
encoderForType (TypeName typeName) typeDef =
  case typeDef of
    Alias elmType -> elmEncoder elmType
    Custom constructors ->
      lambda $ matchVar lowerCasedTypeName $ \x ->
        mkCase x $ toList $ matchConstructor <$> constructors
      where
        lowerCasedTypeName =
          case T.uncons typeName of
            Nothing -> mempty
            Just (x, rest) -> T.cons (Char.toLower x) rest
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
                (fn1 (var _Json_Encode_object) . list . fmap snd)
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
                  zipWith (\i param -> ("param" <> (T.pack $ show i), param)) [(1 :: Int) ..] params

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
        recordLambda' :: [T.Text] -> Record -> ElmValue r
        recordLambda' [] record = mkRecord record
        recordLambda' (name : rest) record =
          anyType $ lambda $ matchVar name $ \x -> recordLambda' rest (addField name x record)
    Lambda _ _ -> error "Cannot decode lambda function from JSON"
    Cmd _ -> error "Cannot decode Cmd"
    Defined name _ -> v (decoderNameForType name)

decodeMapN :: [ElmValue (Decoder g)] -> ElmValue (Decoder value) -> ElmValue (Decoder value)
decodeMapN decoders fn =
  case decoders of
    [] -> fn1 (var _Json_Decode_succeed) (anyType fn)
    [d] -> d |> fn1 (var _Json_Decode_map) (anyType fn)
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

decoderNameForType :: TypeName -> T.Text
decoderNameForType (TypeName name) =
  "decoder" <> name

decoderForType :: ElmTypeDefinition -> ElmValue (Decoder Any)
decoderForType typeDef =
  case typeDef of
    Alias elmType -> elmDecoder elmType
    Custom constructors ->
      fn2 (var _Json_Decode_field) "ctor" (var _Json_Decode_string)
        |> fn1
          (var _Json_Decode_andThen)
          ( lambda $ matchVar "ctor" $ \ctor ->
              fn1 (var _Json_Decode_field) "val"
                <| ( mkCase ctor $
                       (toList $ decodeConstructor <$> constructors) <> [catchAll]
                   )
          )
      where
        catchAll = matchVar "_" $ \_ -> fn1 (var _Json_Decode_fail) "Unexpected constructor"
        decodeConstructor :: (Wire.ConstructorName, [ElmType]) -> (Pattern a, ElmValue (Decoder Any))
        decodeConstructor (Wire.ConstructorName ctorName, params) =
          ( matchString ctorName,
            decodeMapN (elmDecoder <$> params) (var (fromVarName ctorName))
          )

generateClient :: Endpoint -> ElmFunction
generateClient endpoint =
  ElmFunction
    { fnName = endpointFunctionName endpoint,
      fnType = Fix $ Lambda inputRec $ Fix $ Cmd $ Fix $ Result (Fix _Http_Error) $ fromWireType $ responseBody endpoint,
      fnImplementation =
        lambda $
          matchRecordN
            (Wire.unFieldName . fst <$> fields)
            (anyType . var)
            ( \_options ->
                fn1
                  (var _Http_request)
                  ( emptyRecord
                      & addField "method" (string $ TE.decodeUtf8 $ method endpoint)
                      & addField "headers" (list $ uncurry headerOption <$> headers endpoint)
                      & addField "url" (urlOption endpoint)
                      & addField "body" bodyOption
                      & addField "expect" expectOption
                      & addField "timeout" (var _Nothing)
                      & addField "tracker" (var _Nothing)
                      & mkRecord
                  )
            )
    }
  where
    _Http_Error = Defined "Http.Error" []
    inputRec :: ElmType
    inputRec = Fix $ Record $ fields
    fields =
      mconcat
        [ maybeToList $ bodyField <$> body endpoint,
          uncurry headerField <$> headers endpoint,
          uncurry queryField <$> query endpoint,
          paramFields (path endpoint)
        ]
    bodyField :: Wire.Type_ -> (Wire.FieldName, ElmType)
    bodyField wireType = ("body", fromWireType wireType)
    expectOption :: ElmValue (Expect (Result Error Any))
    expectOption =
      fn2
        (var _Http_expectJson)
        (var _identity)
        (elmDecoder (fromWireType (responseBody endpoint)))
    bodyOption :: ElmValue Body
    bodyOption =
      case body endpoint of
        Nothing ->
          var _Http_emptyBody
        Just type_ ->
          var (fromVarName "body")
            |> elmEncoder (fromWireType type_)
            |> var _Http_jsonBody
    headerOption :: T.Text -> Parameter.Parameter -> ElmValue Header
    headerOption name param =
      fn2
        (var _Http_header)
        (string name)
        (paramToString param (var (fromVarName (toCamelCase name))))
    headerField :: T.Text -> Parameter.Parameter -> (Wire.FieldName, ElmType)
    headerField name val = (toFieldName name, elmTypeForParameter val)
    queryField :: T.Text -> QueryVal -> (Wire.FieldName, ElmType)
    queryField name val =
      ( toFieldName name,
        case val of
          QueryFlag -> Fix Bool
          QueryParam param -> elmTypeForParameter param
          QueryList param -> Fix (List (elmTypeForParameter param))
      )
    paramFields :: Path -> [(Wire.FieldName, ElmType)]
    paramFields path =
      case path of
        Static _ rest -> paramFields rest
        Capture name param rest ->
          (toFieldName name, elmTypeForParameter param)
            : paramFields rest
        CaptureAll name param ->
          [ ( toFieldName name,
              Fix (List (elmTypeForParameter param))
            )
          ]
        Root -> []
    urlOption :: Endpoint -> ElmValue String
    urlOption endpoint' =
      fn2
        (var _Url_Builder_absolute)
        (list (pathSegments (path endpoint')))
        (fn1 (var _List_concat) (list (uncurry querySegment <$> query endpoint')))
    querySegment :: T.Text -> QueryVal -> ElmValue [QueryParameter]
    querySegment name val =
      case val of
        QueryFlag ->
          list
            [ fn2
                (var _Url_Builder_string)
                (string name)
                $ ifThenElse
                  (var (fromVarName (toCamelCase name)))
                  "true"
                  "false"
            ]
        QueryParam param ->
          list
            [ queryParam
                name
                param
                (var (fromVarName (toCamelCase name)))
            ]
        QueryList param ->
          fn2
            (var _List_map)
            ( lambda $ matchVar "x" $ \x ->
                queryParam
                  (name <> "[]")
                  param
                  x
            )
            (var (fromVarName (toCamelCase name)))
    pathSegments :: Path -> [ElmValue String]
    pathSegments path =
      case path of
        Static name rest -> string name : pathSegments rest
        Capture name param rest -> paramToString param (var (fromVarName (toCamelCase name))) : pathSegments rest
        CaptureAll name param ->
          [ fn2
              (var _List_map)
              (lambda $ matchVar "x" (paramToString param))
              (var (fromVarName (toCamelCase name)))
              |> fn1 (var _String_join) (string "/")
          ]
        Root -> []

toFieldName :: T.Text -> Wire.FieldName
toFieldName = Wire.FieldName . toCamelCase

toCamelCase :: T.Text -> T.Text
toCamelCase name =
  case parts of
    [] -> ""
    first' : rest -> T.toLower first' <> foldMap T.toTitle rest
  where
    parts = filter (not . T.null) $ T.split (not . Char.isLetter) name

elmTypeForParameter :: Parameter.Parameter -> ElmType
elmTypeForParameter param =
  case Parameter.wrapper param of
    Just (name, _) -> Fix (Defined (toElmTypeName name) [])
    Nothing ->
      case Parameter.type_ param of
        Parameter.Int -> Fix Int
        Parameter.String -> Fix String

paramToString :: Parameter.Parameter -> ElmValue Any -> ElmValue String
paramToString = withPrimitive headerPrimitive

withPrimitive :: (Parameter.Primitive -> ElmValue b -> ElmValue a) -> Parameter.Parameter -> ElmValue b -> ElmValue a
withPrimitive f param val =
  case Parameter.wrapper param of
    Nothing -> f (Parameter.type_ param) val
    Just (_, ctor) ->
      fn1
        ( lambda $
            matchCtor1
              (fromVarName ctor)
              (nameOfPrimitive (Parameter.type_ param))
              (f (Parameter.type_ param))
        )
        val

nameOfPrimitive :: Parameter.Primitive -> T.Text
nameOfPrimitive primitive =
  case primitive of
    Parameter.Int -> "int"
    Parameter.String -> "string"

headerPrimitive :: Parameter.Primitive -> ElmValue Any -> ElmValue (String)
headerPrimitive primitive val =
  case primitive of
    Parameter.Int -> fn1 (var _String_fromInt) (anyType val)
    Parameter.String -> anyType val

queryParam :: T.Text -> Parameter.Parameter -> ElmValue Any -> ElmValue QueryParameter
queryParam key =
  withPrimitive (queryPrimitive key)

queryPrimitive :: T.Text -> Parameter.Primitive -> ElmValue Any -> ElmValue QueryParameter
queryPrimitive key primitive val =
  case primitive of
    Parameter.Int -> fn2 (var _Url_Builder_int) (string key) (anyType val)
    Parameter.String -> fn2 (var _Url_Builder_string) (string key) (anyType val)

endpointFunctionName :: Endpoint -> T.Text
endpointFunctionName endpoint =
  method' <> foldMap T.toTitle (reverse $ segments $ path endpoint)
  where
    method' =
      T.toLower $ TE.decodeUtf8 (method endpoint)
    segments path' =
      case path' of
        Static name rest -> name : segments rest
        Capture name _ rest -> name : segments rest
        CaptureAll name _ -> [name]
        Root -> []
