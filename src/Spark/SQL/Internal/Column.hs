{-# LANGUAGE MagicHash #-}
module Spark.SQL.Internal.Column where

import Java
import Spark.SQL.Internal.Types

foreign import java unsafe alias :: JString -> Java Column Column

foreign import java unsafe and :: Column -> Java Column Column

foreign import java unsafe apply :: Object -> Java Column Column

foreign import java unsafe as :: (u <: Object) => Encoder u -> Java Column (TypedColumn Object u)

-- foreign import java unsafe as2 :: Seq JString -> Java Column Column

foreign import java unsafe as2 :: JString -> Java Column Column

foreign import java unsafe as3 :: JStringArray -> Java Column Column

foreign import java unsafe as4 :: JString -> Metadata -> Java Column Column

-- foreign import java unsafe as5 :: Symbol -> Java Column Column

foreign import java unsafe asc_nulls_first :: Java Column Column

foreign import java unsafe asc_nulls_last :: Java Column Column

foreign import java unsafe asc :: Java Column Column

foreign import java unsafe between :: Object -> Object -> Java Column Column

foreign import java unsafe bitwiseAND :: Object -> Java Column Column

foreign import java unsafe bitwiseOR :: Object -> Java Column Column

foreign import java unsafe bitwiseXOR :: Object -> Java Column Column

foreign import java unsafe cast :: DataType -> Java Column Column

foreign import java unsafe cast2 :: JString -> Java Column Column

foreign import java unsafe contains :: Object -> Java Column Column

foreign import java unsafe desc_nulls_first :: Java Column Column

foreign import java unsafe desc_nulls_last :: Java Column Column

foreign import java unsafe desc :: Java Column Column

foreign import java unsafe divide :: Object -> Java Column Column

foreign import java unsafe endsWith :: Column -> Java Column Column

foreign import java unsafe endsWith2 :: JString -> Java Column Column

foreign import java unsafe eqNullSafe :: Object -> Java Column Column

foreign import java unsafe equals :: Object -> Java Column Bool

foreign import java unsafe equalTo :: Object -> Java Column Column

foreign import java unsafe explain :: Bool -> Java Column ()

foreign import java unsafe expr :: Java Column Expression

foreign import java unsafe geq :: Object -> Java Column Column

foreign import java unsafe getField :: JString -> Java Column Column

foreign import java unsafe getItem :: Object -> Java Column Column

foreign import java unsafe gt :: Object -> Java Column Column

foreign import java unsafe hashcode :: Java Column Int

foreign import java unsafe isin :: JObjectArray -> Java Column Column

-- foreign import java unsafe isin2 :: Seq Object -> Java Column Column

foreign import java unsafe isNaN :: Java Column Column

foreign import java unsafe isNotNull :: Java Column Column

foreign import java unsafe isNull :: Java Column Column

foreign import java unsafe leq :: Object -> Java Column Column

foreign import java unsafe like :: JString -> Java Column Column

foreign import java unsafe lt :: Object -> Java Column Column

foreign import java unsafe minus :: Object -> Java Column Column

foreign import java unsafe mod :: Object -> Java Column Column

foreign import java unsafe multiply :: Object -> Java Column Column

foreign import java unsafe name :: JString -> Java Column Column

foreign import java unsafe notEqual :: Object -> Java Column Column

foreign import java unsafe or :: Column -> Java Column Column

foreign import java unsafe otherwise :: Object -> Java Column Column

foreign import java unsafe over :: Java Column Column

foreign import java unsafe over2 :: WindowSpec -> Java Column Column

foreign import java unsafe plus :: Object -> Java Column Column

foreign import java unsafe rlike :: JString -> Java Column Column

foreign import java unsafe startsWith :: Column -> Java Column Column

foreign import java unsafe startsWith2 :: JString -> Java Column Column

foreign import java unsafe substr :: Column -> Column -> Java Column Column

foreign import java unsafe substr2 :: Int -> Int -> Java Column Column

foreign import java unsafe toString :: Java Column JString

foreign import java unsafe when :: Column -> Object -> Java Column Column
