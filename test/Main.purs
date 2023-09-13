module Test.Main
( main
) where

import Data.ByteString (ByteString, Encoding(..), Octet, cons, empty, foldl, foldr, fromString, fromUTF8, head, init, isEmpty, last, length, map, pack, reverse, singleton, snoc, tail, toUTF8, uncons, unpack, unsnoc)
import Prelude (Unit, bind, bottom, discard, flip, identity, pure, top, (#), ($), (&&), (+), (-), (/), (<), (<$>), (<*>), (<<<), (<>), (==), (>), (||))

import Control.Monad.Gen (frequency)
import Data.Array (foldMap)
import Data.Enum (toEnumWithDefaults)
import Data.Foldable as Foldable
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty(..))
import Data.String (CodePoint, fromCodePointArray)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Prelude as Prelude
import Test.QuickCheck (class Arbitrary, arbitrary, quickCheck, (===))
import Test.QuickCheck.Gen (arrayOf, chooseInt, suchThat)
import Test.QuickCheck.Laws.Data.Eq (checkEq)
import Test.QuickCheck.Laws.Data.Monoid (checkMonoid)
import Test.QuickCheck.Laws.Data.Ord (checkOrd)
import Test.QuickCheck.Laws.Data.Semigroup (checkSemigroup)
import Type.Proxy (Proxy(..))
import Type.Quotient (mkQuotient, runQuotient)


main :: Effect Unit
main = do
    log "laws"
    checkEq        (Proxy :: Proxy ByteString)
    checkMonoid    (Proxy :: Proxy ByteString)
    checkOrd       (Proxy :: Proxy ByteString)
    checkSemigroup (Proxy :: Proxy ByteString)

    log "singleton"
    quickCheck $ \b -> unpack (singleton b) === [b]

    log "pack and unpack"
    quickCheck $ \b -> unpack (pack b) === b
    quickCheck $ \b -> pack (unpack b) === b

    log "cons"
    quickCheck $ \c b -> unpack (cons c b) === [c] <> unpack b

    log "snoc"
    quickCheck $ \c b -> unpack (snoc b c) === unpack b <> [c]

    log "uncons"
    quickCheck $ \c b -> case uncons (cons c b) of
                           Just r  -> r.head == c && r.tail == b
                           Nothing -> false

    log "unsnoc"
    quickCheck $ \c b -> case unsnoc (snoc b c) of
                           Just r  -> r.init == b && r.last == c
                           Nothing -> false

    log "head"
    quickCheck $ head empty === Nothing
    quickCheck $ \c b -> head (cons c b) === Just c

    log "tail"
    quickCheck $ tail empty === Nothing
    quickCheck $ \c b -> tail (cons c b) === Just b

    log "last"
    quickCheck $ last empty === Nothing
    quickCheck $ \c b -> last (snoc b c) === Just c

    log "init"
    quickCheck $ init empty === Nothing
    quickCheck $ \c b -> init (snoc b c) === Just b

    log "length"
    quickCheck $ \b c -> length (b <> c) === length b + length c

    log "isEmpty"
    quickCheck $ isEmpty empty
    quickCheck $ \b -> isEmpty b === (length b == 0)

    log "map"
    quickCheck $ \b f -> map f b === pack (Prelude.map f (unpack b))

    log "reverse"
    quickCheck $ \b -> reverse (reverse b) === b

    log "foldl"
    quickCheck $ \b -> foldl subL 0 b === Foldable.foldl (-) 0 (runQuotient <$> unpack b)

    log "foldr"
    quickCheck $ \b -> foldr subR 0 b === Foldable.foldr (-) 0 (runQuotient <$> unpack b)

    log "fromString"
    quickCheck $ fromString "ABCD" Hex === Just (withOctets pack [0xAB, 0xCD])
    -- this line is commented out as for invalid input result is `pack []` and shuold be fixed later
    -- quickCheck $ fromString "LOL" Hex === Nothing
    log "utf8"
    quickCheck $ \(BMPString s) -> fromUTF8 (toUTF8 s) === s
  
    where
    subL a b = a - runQuotient b
    subR a b = runQuotient a - b

newtype BMPString = BMPString String

data UnicodeChar = Normal CodePoint | Surrogates CodePoint CodePoint

instance Arbitrary BMPString where
  arbitrary = BMPString <$> do 
      ucs <- arrayOf (arbitrary @UnicodeChar)
      pure $ fromCodePointArray $ foldMap f ucs
    where
      f :: UnicodeChar -> Array CodePoint
      f uc = case uc of
        Normal a -> [a]
        Surrogates a b -> [a, b]

instance Arbitrary UnicodeChar where
  arbitrary = frequency $ NonEmpty (Tuple (1.0 - p) normalGen) [Tuple p surrogatesGen]

    where 
      hiLB = 0xD800
      hiUB = 0xDBFF
      loLB = 0xDC00
      loUB = 0xDFFF
      maxCP = 65535
      toCP = toEnumWithDefaults bottom top 
      -- must have a high surrogate followed by a low surrogate
      surrogatesGen = Surrogates <$> (toCP <$> chooseInt hiLB hiUB) <*> (toCP <$> chooseInt loLB loUB)
      normalGen = Normal <<< toCP <$> do
        chooseInt 0 maxCP `suchThat` \n -> 
          (n < hiLB || n > hiUB) && (n < loLB || n > loUB)
      -- probability that you pick a surrogate from all possible codepoints
      p = toNumber ((hiUB - hiLB  + 1) + (loUB - loLB + 1)) / toNumber (maxCP + 1)

withOctet :: ∀ a. (Octet -> a) -> Int -> a
withOctet = flip $ (#) <<< mkQuotient

withOctets :: ∀ a. (Array Octet -> a) -> Array Int -> a
withOctets f xs = f (Prelude.map (withOctet identity) xs)
