-- https://www.youtube.com/watch?v=r_Enynu_TV00

import Text.ParserCombinators.Parsec hiding ((<|>), many)
import Control.Applicative
import Control.Monad

-- parse matchTrue "test" "true"  -> Right True
-- parse matchTrue "test" "lemon" -> Left "test" expecting "true"
matchTrue :: Parser String
matchTrue = string "true"

-- parse alwaysTrue "test" "true"  -> Right True
-- parse alwaysTrue "test" "lemon" -> Right True
-- parse alwaysTrue "test" "flase" -> Right True 
alwaysTrue :: Parser Bool
alwaysTrue = pure True

-- parse boolTrue "test" "true" -> Right True
-- parse boolTrue "test" "lemon -> Left "test" expecting "true"
boolTrue :: Parser Bool
boolTrue = matchTrue *> alwaysTrue

boolFalse :: Parser Bool
boolFalse = (string "false") *> (pure False)

-- parse bool "test" "true"  -> Right True
-- parse bool "test" "false" -> Right False
-- parse bool "test" "lemon" -> Left "test" expecting "true" or "false"
bool :: Parser Bool
bool = boolTrue <|> boolFalse
