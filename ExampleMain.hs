{-
Name: Josh Sellers
File Description: Example
-}

{-# OPTIONS -Wall -Wno-unused-imports #-}

module Example where

import Types
import Functions
import Game
import PokerExample

orderRank :: [(Rank, Integer)]
orderRank = orderAutomatic r 
      where r = [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace]

orderSuit :: [(Suit, Integer)]
orderSuit = orderManual s o 
      where s = [Clubs, Diamonds, Hearts, Spades]
            o = [1,1,1,1]

handSize :: Integer
handSize = 5

scoreHand :: Game -> [Card] -> Integer
scoreHand = undefined

scorePile :: Game -> [Card] -> Integer
scorePile = undefined


compareHands :: Game -> [Card] -> [Card] -> Ordering
compareHands gm c1 c2 = handComp gm c1 c2


game :: Game
game = Game {_orderRank = orderRank, _orderSuit = orderSuit, _scoreHand = scoreHand, 
             _compareHands = compareHands,  _scorePile = scorePile, _handSize = handSize}

poker :: Game -> Dealer -> IO ()
poker gm d = do deck <- shuffle (_deck d)
                let dealer = d {_deck = deck}
                let newDealer = deals dealer (fromIntegral (_handSize gm) :: Int)
                putStrLn (show (bestHand gm (_players newDealer)))
                return ()

main :: IO()
main = do let p1 = Player {_hand = [], _storedHands=[],_turn=0,_score=0,_name="Bob",_played=[]}
          let p2 = Player {_hand = [], _storedHands=[],_turn=0,_score=0,_name="Joe",_played=[]}
          let d = Dealer {_nameD="Jeff",_turnD=0,_scoreD=0,_storedHandsD=[],_deck=fullDeck,
                          _discard=[],_players=[p1,p2],_handD=[],_playedD=[]} 
          poker game d