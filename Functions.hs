{-
Name: Josh Sellers
File Description: Functions
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS -Wall -Wno-unused-imports #-}

module Functions where

import Data.Either
import Data.List
import Data.Maybe
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import Data.STRef
--import Control.Monad.Random
import System.Random
import GHC.Arr


import Types

{- Deck Functions -}

-- full deck
fullDeck :: [Card]
fullDeck = Card <$> [minBound..] <*> [minBound..]

-- if you don't want a full deck
partialDeck :: [Card] -> [Card]
partialDeck remove = fullDeck \\ remove

-- empty list
initialDiscardPile :: [Card]
initialDiscardPile = []

-- go back to how they were
revertDrawDiscard :: Dealer -> Dealer
revertDrawDiscard dealer = dealer {_deck = d, _discard = []}
          where d = (_deck dealer) ++ (_discard dealer)

-- find card in deck
getCard :: [Card] -> Card -> [Card]
getCard [] _ = []
getCard (d:deck) c 
        | d == c    = [d]
        | otherwise = getCard deck c

-- remove specific card from deck -> return card and new deck
removeCard :: [Card] -> Card -> ([Card],[Card])
removeCard [] _ = ([], [])
removeCard deck c = ((getCard deck c),(deck \\ [c])) 

-- draw specific cards from deck -> return cards and new deck
drawSpecificCards :: [Card] -> [Card] -> ([Card],[Card])
drawSpecificCards deck [] = ([], deck)
drawSpecificCards [] _ = ([], [])
drawSpecificCards deck cards = ((getSpecific deck cards), (deck \\ cards))
        where getSpecific [] _ = []
              getSpecific _ [] = []
              getSpecific dck (c:cds) = getCard dck c ++ getSpecific (dck \\ [c]) cds

-- https://www.reddit.com/r/haskell/comments/96ic6c/how_do_i_shuffle_a_list/
-- I think this should work since the game would be run through a main method
shuffle :: [Card] -> IO [Card]
shuffle [] = return []
shuffle [x] = return [x] 
shuffle deck = do
    i <- randomRIO (0,length deck - 1)
    shuffledRest <- shuffle (take i deck ++ drop (i+1) deck)
    return $ (deck !! i) : shuffledRest

{- Drawing Functions -}

class Drawing c where
   draw :: Dealer -> c -> ([Card], Dealer)
instance Drawing Int where
    draw dealer 0 = ([],dealer)
    draw dealer n = ((take n deck), d)
        where deck = _deck dealer
              d = dealer {_deck = (drop n deck)}
instance Drawing Card where
    draw dealer c = (card, d)
        where out = removeCard (_deck dealer) c 
              d = dealer {_deck = (snd out)}
              card = fst out
instance Drawing [Card] where
    draw dealer cards = (cs, d)
        where out = drawSpecificCards (_deck dealer) cards
              cs = fst out
              d = dealer {_deck = (snd out)}


{- Deal Functions -}

class (Drawing n) => Dealing p n where
    deal :: Dealer -> p -> n -> Dealer
instance (Drawing n) => Dealing Dealer n where
    deal d1 _ x = dealer
        where (cards, dlr) = draw d1 x
              dealer = dlr {_handD = cards ++ (_handD dlr)}
instance (Drawing n) => Dealing Player n where
    deal dealer player x = d
        where (cards, dlr) = draw dealer x
              dl = dlr 
              p = player {_hand = cards ++ (_hand player)}
              d = dl {_players = replace (_players dl) player p}

dealToDealer :: (Drawing n) => Dealer -> n -> Dealer
dealToDealer dealer n = deal dealer dealer n

deals :: (Drawing n) => Dealer -> n -> Dealer
deals dealer n = foldl (\ b x -> deal b x n) dealer (_players dealer)  

{- Play Functions -}

class Playing p n where
    play :: Table -> p -> n -> (p, Table)
instance Playing Dealer Int where
    play table dealer 0 = (dealer, table)
    play table dealer n = (d,t)
        where hand = _handD dealer
              pile = _inPlay table
              d = dealer {_handD = (drop n hand)}
              t = table {_inPlay = (take n hand) ++ pile}
instance Playing Dealer Card where
    play table dealer card = (d,t)
        where hand = _handD dealer
              pile = _inPlay table
              (c, h) = removeCard hand card 
              d = dealer {_handD = h}
              t = table {_inPlay = c ++ pile}
instance Playing Dealer [Card] where
    play table dealer cards = (d,t)
        where hand = _handD dealer
              pile = _inPlay table
              (cs,h) = drawSpecificCards hand cards
              d = dealer {_handD = h}
              t = table {_inPlay = cs ++ pile}
instance Playing Player Int where
    play table player 0 = (player, table)
    play table player n = (p,t)
        where hand = _hand player
              pile = _inPlay table
              p = player {_hand = (drop n hand)}
              t = table {_inPlay = (take n hand) ++ pile}
instance Playing Player Card where
    play table player card = (p,t)
        where hand = _hand player
              pile = _inPlay table
              (c, h) = removeCard hand card 
              p = player {_hand = h}
              t = table {_inPlay = c ++ pile}
instance Playing Player [Card] where
    play table player cards = (p,t)
        where hand = _hand player
              pile = _inPlay table
              (cs,h) = drawSpecificCards hand cards
              p = player {_hand = h}
              t = table {_inPlay = cs ++ pile}


{- Discard Functions -}      

class Discarding c where
   discardP :: Dealer -> Player -> c -> (Dealer, Player)
   discardD :: Dealer -> c -> Dealer
   discardT :: Dealer -> Table -> c -> (Dealer, Table)
instance Discarding Int where
    discardP dealer player 0 = (dealer, player)
    discardP dealer player n = (d,p)
             where hand = _hand player
                   d = dealer {_discard = ((take n hand) ++ (_discard dealer))}
                   p = player {_hand = (drop n hand) }
    discardD dealer 0 = dealer
    discardD dealer n = d
            where h = _handD dealer
                  d = dealer {_discard = ((take n h) ++ (_discard dealer)), _handD = (drop n h) }
    discardT dealer table 0 = (dealer, table)
    discardT dealer table n = (d,t)
            where pile = _inPlay table
                  d = dealer {_discard = ((take n pile) ++ (_discard dealer))}
                  t = table {_inPlay = (drop n pile)}
instance Discarding Card where
    discardP dealer player card = (d, p)
            where hand = _hand player
                  d = dealer {_discard = (getCard hand card) ++ (_discard dealer)} 
                  p = player {_hand = (snd (removeCard hand card))}
    discardD dealer card = d
            where hand = _handD dealer
                  d = dealer {_discard = (getCard hand card) ++ (_discard dealer), _handD = (snd (removeCard hand card))} 
    discardT dealer table card = (d, t)
            where hand = _inPlay table
                  d = dealer {_discard = (getCard hand card) ++ (_discard dealer)} 
                  t = table {_inPlay = (snd (removeCard hand card))}
instance Discarding [Card] where
    discardP dealer player [] = (dealer, player)
    discardP dealer player cards = (d,p)
            where hand = _hand player
                  (c, h) = drawSpecificCards hand cards
                  p = player {_hand = h}
                  d = dealer {_discard = c ++ (_discard dealer)}
    discardD dealer [] = dealer
    discardD dealer cards = d
            where hand = _handD dealer
                  (c, h) = drawSpecificCards hand cards
                  d = dealer {_discard = c ++ (_discard dealer), _handD = h}
    discardT dealer table [] = (dealer, table)
    discardT dealer table cards = (d,t)
            where hand = _inPlay table
                  (c, h) = drawSpecificCards hand cards
                  t = table {_inPlay = h}
                  d = dealer {_discard = c ++ (_discard dealer)}

{- Fold Functions -}

class Folding p where
    fold :: Dealer -> p -> (Dealer, p)
instance Folding Dealer where
    fold d1 _ = (d1 {_discard = d ,_handD = []},d1 {_discard = d ,_handD = []})
            where dis = _discard d1
                  hand =_handD d1
                  d = hand ++ dis
instance Folding Player where
    fold dealer player = (d,p)
            where dis = _discard dealer
                  hand =_hand player
                  d = dealer {_discard = hand ++ dis}
                  p = player {_hand = []}
instance Folding Table where
    fold dealer table = (d,t) 
            where dis = _discard dealer
                  hand =_inPlay table
                  d = dealer {_discard = hand ++ dis}
                  t = table {_inPlay = []}

clearTable :: Dealer -> Table -> (Dealer, Table)
clearTable dealer table = fold dealer table

clearDealer :: Dealer -> Dealer
clearDealer dealer = fst $ fold dealer dealer

{- Table Functions -}

changeTableScore :: Table -> Integer -> Table
changeTableScore table n = table {_pointsInPlay = n + (_pointsInPlay table)}

{- Player Functions -}

-- increase score of player
changePlayerScore :: Player -> Integer -> Player
changePlayerScore player n = player {_score = x}
  where x = (_score player) + n

{- Dealer Functions -}

-- increase score of dealer
changeDealerScore :: Dealer -> Integer -> Dealer
changeDealerScore dealer n = dealer {_scoreD = x}
  where x = (_scoreD dealer) + n

-- add player to dealer 
addPlayer :: Dealer -> Player -> Dealer
addPlayer dealer player = if (elem player (_players dealer))
                              then dealer 
                              else dealer {_players = x}
                               where x = player : (_players dealer)

-- remove player to dealer 
removePlayer :: Dealer -> Player -> Dealer
removePlayer dealer player = if (elem player (_players dealer))
                                then dealer {_players = x}
                                else dealer
                                  where x = (_players dealer) \\ [player]

{- Extra Functions -}

replace :: Eq a =>[a] -> a -> a -> [a]
replace [] _ _ = []
replace (x:xs) y z = if (y==x) then z:xs else x : replace xs y z

