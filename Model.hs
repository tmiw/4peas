module Model where

import Prelude
import Yesod
import Data.Text (Text)
import Database.Persist.Quasi
import Data.Time (UTCTime)
import Database.Persist.Store

-- Valid ingredient units.
data IngredientUnit = 
    None |
    Teaspoon | 
    Tablespoon | 
    Cup | 
    Milliliter | 
    Liter | 
    Gram | 
    Kilogram | 
    Pound | 
    Ounce 
    deriving (Eq, Show, Enum);

instance PersistField IngredientUnit where
    toPersistValue = PersistInt64 . fromIntegral . fromEnum
    fromPersistValue (PersistInt64 a) = Right $ toEnum $ fromIntegral a
    fromPersistValue _ = Left "Invalid field data."
    sqlType _ = SqlInteger
    isNullable _ = False

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
