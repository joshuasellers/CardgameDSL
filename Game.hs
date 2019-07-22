{-
Name: Josh Sellers
File Description: Rules
-}

{-# OPTIONS -Wall -Wno-unused-imports #-}

module Game where

import Data.Either
import Data.List
import Data.Maybe
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import Data.STRef
import System.Random

import Types
import Functions

{- Helper Functions For Rules and Scoring -}

-- combine suit and rank into card ranknig

fullCardRanking :: Game -> [(Card, Integer)]
fullCardRanking game = c
          where s = sortBy sortGT (_orderSuit game)
                r = sortBy sortGT (_orderRank game)
                c = combine r s
                sortGT (_, b1) (_, b2)
                     | b1 < b2 = GT
                     | b1 > b2 = LT
                     | b1 == b2 = EQ

combine :: [(Rank, Integer)] -> [(Suit, Integer)] -> [(Card, Integer)]
combine [] _ = []
combine _ [] = []
combine (r:ranks) s = (foldl (\ b x -> if (((snd $ (last s)) - (snd $ (head s))) == 0)
                                          then b ++ [(Card {_rank = fst r, _suit = fst x}, snd r)]
                                          else b ++ [(Card {_rank = fst r, _suit = fst x}, 4 * snd r + snd x)]) 
                                         [] s) ++ combine ranks s



-- get int ranking of card

getCardRank :: Game -> Card -> Integer
getCardRank game card = case (find (matchFirst card) (fullCardRanking game)) of
       Nothing -> -1
       Just (_,i) -> i
      where matchFirst x (c, _) = x == c

-- convert hand to list of int ranking numbers
handToListNum :: Game -> [Card] -> [Integer]
handToListNum _ [] = []
handToListNum game (x:xs) = (getCardRank game x) : (handToListNum game xs)

-- convert another player to the dealer
toDealer :: Dealer -> Player -> (Dealer, Dealer)
toDealer currdealer player = (newD, oldD)
    where newD = Dealer {_nameD = (_name player), _turnD = (_turn player),
                    _scoreD = (_score player), _storedHandsD = (_storedHands player), _deck = (_deck currdealer),
                    _discard = (_discard currdealer), _players = (_players currdealer) \\ [player],
                    _handD = (_hand player), _playedD = (_played player)}
          oldD = currdealer {_players = (_players newD)}

-- convert dealer to player
toPlayer :: Dealer -> Player
toPlayer dealer = Player {_hand = (_handD dealer), _storedHands = (_storedHandsD dealer),
        _turn = (_turnD dealer), _score = (_scoreD dealer), _name = (_nameD dealer), 
        _played = (_playedD dealer)}

-- error check for if a list of cards has dupes
hasDupes :: [Card] -> Bool
hasDupes [] = False
hasDupes (c:cards) =  if (elem c cards) then True else hasDupes cards 

-- all possible pairs in a hand
pairs :: [Card] -> [Card]
pairs [] = []
pairs (x:xs) = foldr (\ c b -> if (_rank c) == (_rank x) then [c] ++ b else b) [] xs ++ (pairs xs)


orderManual :: [a] -> [Integer] -> [(a,Integer)]
orderManual [] _ = error "Empty list error"
orderManual _ [] = error "Empty list error"
orderManual as (o:ords) = if (last $ sort (o:ords)) - (head $ sort (o:ords)) == (toInteger (length (o:ords))) - 1 && (head $ sort (o:ords)) == 0
                           then order as (o:ords) 
                           else if (last $ sort (o:ords)) == (head $ sort (o:ords)) || ((last $ sort (o:ords)) < (toInteger $ length (o:ords)) && (head $ sort (o:ords)) == 0) 
                                 then order as (o:ords)
                                 else error "give sequential values starting at 0"
                  where order [] [] = []
                        order [] _ = error "Empty list error"
                        order _ [] = error "Empty list error"
                        order (x:xs) (y:ys) = if ((length xs) == (length ys))
                                              then (x,y) : order xs ys
                                              else error "Lists need to be the same length"

orderAutomatic :: [a] -> [(a,Integer)]
orderAutomatic [] = error "Empty list error"
orderAutomatic as = orderManual as ords
            where ords = [0..(toInteger (length as) - 1)]


-- https://stackoverflow.com/questions/35118659/haskell-permutations-with-the-length-of-the-output-list
possibleHands :: Int -> [Card] -> [[Card]]
possibleHands n deck = concatMap permutations $ possibleHands' deck [] where
  possibleHands' []     r = if length r == n then [r] else []
  possibleHands' (x:xs) r | length r == n = [r]
                          | otherwise     = possibleHands' xs (x:r) 
                                            ++ possibleHands' xs r

bestHand :: Game -> [Player] -> Player
bestHand _ [] = undefined
bestHand game (p:players) = foldl (\ b x -> if ((_compareHands game) game (_hand b) (_hand x)) == GT then b else x) p players
