{-# LANGUAGE OverloadedStrings #-}
module Backend.Orphans where

import Data.CharacterSheet
import Data.Text (Text)
import qualified Data.Text as T
import Database.Esqueleto

-- ### Database fields ###
instance PersistField SavingThrow where
    toPersistValue = PersistInt64 . fromIntegral . fromEnum
    fromPersistValue (PersistInt64 x) = Right . toEnum . fromIntegral $ x
    fromPersistValue _ = Left "PersistField.SavingThrow.fromPersistValue called on wrong data type"

instance PersistFieldSql SavingThrow where
    sqlType _ = SqlInt32

instance PersistField SpellComp where
    toPersistValue = PersistInt64 . fromIntegral . fromEnum
    fromPersistValue (PersistInt64 x) = Right . toEnum . fromIntegral $ x
    fromPersistValue _ = Left "PersistField.SavingThrow.fromPersistValue called on wrong data type"

instance PersistFieldSql SpellComp where
    sqlType _ = SqlInt32

instance PersistField GameDuration where
    toPersistValue = PersistInt64 . fromIntegral . fromEnum
    fromPersistValue (PersistInt64 x) = Right . toEnum . fromIntegral $ x
    fromPersistValue _ = Left "PersistField.SavingThrow.fromPersistValue called on wrong data type"

instance PersistFieldSql GameDuration where
    sqlType _ = SqlInt32

instance PersistField Target where
    toPersistValue = PersistInt64 . fromIntegral . fromEnum
    fromPersistValue (PersistInt64 x) = Right . toEnum . fromIntegral $ x
    fromPersistValue _ = Left "PersistField.SavingThrow.fromPersistValue called on wrong data type"

instance PersistFieldSql Target where
    sqlType _ = SqlInt32

instance PersistField Class where
    toPersistValue = PersistText . T.pack . show
    fromPersistValue (PersistText x) = Right . read . T.unpack $ x
    fromPersistValue _ = Left "PersistField.Class.fromPersistValue called on wrong data type"

instance PersistFieldSql Class where
    sqlType _ = SqlString

instance PersistField SpellLevel where
    toPersistValue (SpellLevel x) = PersistInt64 . fromIntegral $ x
    fromPersistValue (PersistInt64 x) = Right . SpellLevel . fromIntegral $ x
    fromPersistValue _ = Left "PersistField.SpellLevel.fromPersistValue called on wrong data type"

instance PersistFieldSql SpellLevel where
    sqlType _ = SqlInt32
