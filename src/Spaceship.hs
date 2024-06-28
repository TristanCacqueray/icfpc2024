module Spaceship where

import RIO
import Data.Attoparsec.Text qualified as P

data V2 = V2 Int Int
  deriving (Show)

parseInput :: Text -> [V2]
parseInput txt = either (error . mappend "oops: ") id $ P.parseOnly (P.many1' coordP <* P.endOfInput) txt

coordP :: P.Parser V2
coordP = do
  i1 <- P.signed P.decimal
  P.skipSpace
  i2 <- P.signed P.decimal
  P.skipSpace
  pure (V2 i1 i2)
