{-
Name: Josh Sellers
File Description: Types
-}

{-# OPTIONS -Wall -Wno-unused-imports #-}

module Types where

import Data.Typeable

-- CARD

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
          | Jack | Queen | King | Ace
  deriving (Read, Eq, Bounded, Enum)
instance Show Rank where
    show x = case x of
     Two   -> "2"
     Three -> "3"
     Four  -> "4"
     Five  -> "5"
     Six   -> "6"
     Seven -> "7"
     Eight -> "8"
     Nine  -> "9"
     Ten   -> "10"
     Jack  -> "Jack"
     Queen -> "Queen"
     King  -> "King"
     Ace   -> "Ace"

data Suit = Clubs | Diamonds | Hearts | Spades
  deriving (Eq, Bounded, Enum)
instance Show Suit where
    show x = case x of
     Clubs    -> " ♧"
     Diamonds -> " ♢"
     Hearts   -> " ♡"
     Spades   -> " ♤"

data Card = Card {_rank :: Rank, _suit :: Suit} 
 deriving (Eq)
instance Show Card where
    show (Card r s) = show r ++ show s

-- PLAYER

data Player = Player {
  _hand :: [Card],
  _storedHands :: [[Card]], 
  _turn :: Integer, 
  _score :: Integer, 
  _name :: String,
  _played :: [Card]} deriving (Eq, Show)

-- DEALER

data Dealer = Dealer {
  _nameD :: String,
  _turnD :: Integer,
  _scoreD :: Integer,
  _storedHandsD :: [[Card]],
  _deck :: [Card], 
  _discard :: [Card], 
  _players :: [Player], 
  _handD :: [Card],
  _playedD :: [Card]} deriving (Eq, Show)

-- TABLE
-- I could potnetially add betting in
data Table = Table {
  _inPlay :: [Card], 
  _pointsInPlay :: Integer} deriving (Eq, Show)

-- GAME
-- check if typeOf actually works TODO
data Game = Game {
  _orderRank :: [(Rank, Integer)], 
  _orderSuit :: [(Suit, Integer)], 
  _scoreHand :: (Game -> [Card] -> Integer),
  _compareHands :: (Game -> [Card] -> [Card] -> Ordering),  
  _scorePile :: (Game -> [Card] -> Integer),
  _handSize :: Integer} 
instance Show Game where
    show (Game oRank oSuit _ _ _ hz) = show oRank ++ show oSuit ++ 
                                          (show "Game -> [Card] -> Integer") ++ (show "Game -> [Card] -> [Card] -> Ordering") 
                                          ++ (show "Game -> [Card] -> Integer") ++ show hz


