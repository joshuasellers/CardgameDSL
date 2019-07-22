{-
Name: Josh Sellers
File Description: Example
-}

{-# OPTIONS -Wall -Wno-unused-imports #-}

module PokerExample where

import Data.Either
import Data.List
import Data.Maybe
import Types
import Game
import Functions

data Hand = HighestCard | Pair | TwoPair | ThreeCards | Straight | Flush | FullHouse
            | FourCards | StraightFlush
    deriving (Read, Show, Eq, Enum, Bounded)

isFlush :: [Card] -> Bool
isFlush [] = False
isFlush (x:xs) = foldr (\ c b -> (b && ((_suit c) == (_suit x)))) True xs

isStraight :: Game -> [Card] -> Bool
isStraight game hand = fst (foldr (\ x (b, m) -> if m == 0
                                            then (b, x)
                                            else (if ((m-x == 1) && b)
                                                        then (True, x)
                                                        else (if ((x == 4 && m == 13) && b)
                                                                    then (True, x)
                                                                    else (False,m ) ) ))
                                                                        (True, 0) (sort (handToListNum game hand)))

getHand :: Game -> [Card] -> Hand
getHand game h
    | ((length (pairs h)) == 1) = Pair
    | ((length (pairs h)) == 2) = TwoPair
    | ((length (pairs h)) == 3) = ThreeCards
    | ((length (pairs h)) == 4) = FullHouse
    | ((length (pairs h)) == 6)= FourCards
    | isStraight game h && isFlush h = StraightFlush
    | isStraight game h = Straight
    | isFlush h = Flush
    | otherwise = HighestCard

deleteCard :: Integer -> [Card] -> [Card]
deleteCard _ [] = []
deleteCard n (x:xs)
    | _rank x == Ace && n == 13 = [] ++ (deleteCard n xs)
    | _rank x == Two && n == 1 = [] ++ (deleteCard n xs)
    | _rank x == Three && n == 2 = [] ++ (deleteCard n xs)
    | _rank x == Four && n == 3 = [] ++ (deleteCard n xs)
    | _rank x == Five && n == 4 = [] ++ (deleteCard n xs)
    | _rank x == Six && n == 5 = [] ++ (deleteCard n xs)
    | _rank x == Seven && n == 6 = [] ++ (deleteCard n xs)
    | _rank x == Eight && n == 7 = [] ++ (deleteCard n xs)
    | _rank x == Nine && n == 8 = [] ++ (deleteCard n xs)
    | _rank x == Ten && n == 9 = [] ++ (deleteCard n xs)
    | _rank x == Jack && n == 10 = [] ++ (deleteCard n xs)
    | _rank x == Queen && n == 11 = [] ++ (deleteCard n xs)
    | _rank x == King && n == 12 = [] ++ (deleteCard n xs)
    | otherwise = [x] ++ (deleteCard n xs)

splitCards :: [Integer] -> (Integer, Integer)
splitCards [] = (0,0)
splitCards hand = (if (snd (fst p)) == (1 :: Integer) then ( (fst (snd p)), (fst (fst p)) ) else ( (fst (fst p)), (fst (snd p)) ) )
  where p = (foldr (\ x ((a,b),(c,d)) -> if a == 0 then ((x,1),(c,d)) else (if c == 0 then ((a,b),(x,1)) else (if x == a then ((x,b+1),(c,d)) else ((a,b),(x,d+1))))) ((0,0),(0,0 :: Integer)) hand)

eqHands :: Game -> Hand -> [Card] -> [Card] -> Ordering
eqHands _ _ [] [] = EQ
eqHands game t hand1 hand2
    | t == HighestCard = (if (last (sort (handToListNum game hand1))) > (last (sort (handToListNum game hand2)))
                            then GT
                            else (if (last (sort (handToListNum game hand1))) < (last (sort (handToListNum game hand2)))
                                    then LT
                                    else eqHands game t (deleteCard (last (sort (handToListNum game hand1))) hand1) (deleteCard (last (sort (handToListNum game hand2))) hand2)))
    | t == Pair = (if (head (handToListNum game (pairs hand1))) > (head (handToListNum game (pairs hand2)))
                        then GT
                        else (if (head (handToListNum game (pairs hand1))) < (head (handToListNum game (pairs hand2)))
                                then LT
                                else (eqHands game HighestCard hand1 hand2)))
    | t == TwoPair = (if (last (sort (handToListNum game (pairs hand1)))) > (last (sort (handToListNum game (pairs hand2))))
                        then GT
                        else (if (last (sort (handToListNum game (pairs hand1)))) < (last (sort (handToListNum game (pairs  hand2))))
                                then LT
                                else eqHands game Pair (deleteCard (last (sort (handToListNum game (pairs hand1)))) hand1) ((deleteCard (last (sort (handToListNum game (pairs hand2)))) hand2)) ))
    | t == ThreeCards = (if (head (handToListNum game (pairs hand1))) > (head (handToListNum game (pairs hand2)))
                            then GT
                            else (if (head (handToListNum game (pairs hand1))) < (head (handToListNum game (pairs hand2)))
                                        then LT
                                        else eqHands game HighestCard hand1 hand2))
    | t == Straight = (if (last (sort (handToListNum game hand1))) > (last (sort (handToListNum game hand2)))
                            then GT
                            else (if (last (sort (handToListNum game hand1))) < (last (sort (handToListNum game hand2)))
                                        then LT
                                        else (if (head (sort (handToListNum game hand1))) == (head (sort (handToListNum game hand2)))
                                                then EQ
                                                else (if (head (sort (handToListNum game hand1))) == 2 then LT else GT))))
    | t == Flush = eqHands game HighestCard hand1 hand2
    | t == FullHouse = (if (fst (splitCards (handToListNum game (pairs hand1)))) > (fst (splitCards (handToListNum game (pairs hand2))))
                            then GT
                            else (if (fst (splitCards (handToListNum game (pairs hand1)))) < (fst (splitCards (handToListNum game (pairs hand2))))
                                    then LT
                                    else (if (snd (splitCards (handToListNum game (pairs hand1)))) > (snd (splitCards (handToListNum game (pairs hand2))))
                                            then GT
                                            else (if (snd (splitCards (handToListNum game (pairs hand1)))) < (snd (splitCards (handToListNum game (pairs hand2))))
                                                    then LT
                                                    else EQ))))
    | t == FourCards = (if (head (handToListNum game (pairs hand1))) > (head (handToListNum game (pairs hand2)))
                            then GT
                            else (if (head (handToListNum game (pairs hand1))) < (head (handToListNum game (pairs hand2)))
                                        then LT
                                        else (eqHands game HighestCard hand1 hand2)) )
    | otherwise = eqHands game Straight hand1 hand2

handComp :: Game -> [Card] -> [Card] -> Ordering
handComp _ [] [] = undefined
handComp _ _ [] = GT
handComp _ [] _ = undefined
handComp game hand1 hand2
    | ((getHand game hand1) == (getHand game hand2)) = eqHands game (getHand game hand1) hand1 hand2
    | ((getHand game hand1) == HighestCard) && (not (getHand game hand2 == HighestCard)) = LT
    | ((getHand game hand1) == Pair) && (not (getHand game hand2 == HighestCard)) = LT
    | ((getHand game hand1) == Pair) && (getHand game hand2 == HighestCard) = GT
    | ((getHand game hand1) == TwoPair) && (elem (getHand game hand2) (enumFrom ThreeCards)) = LT
    | ((getHand game hand1) == TwoPair) && (elem (getHand game hand2) (enumFromTo HighestCard Pair)) = GT
    | ((getHand game hand1) == ThreeCards) && (elem (getHand game hand2) (enumFrom Straight)) = LT
    | ((getHand game hand1) == ThreeCards) && (elem (getHand game hand2) (enumFromTo HighestCard TwoPair)) = GT
    | ((getHand game hand1) == Straight) && (elem (getHand game hand2) (enumFrom Flush)) = LT
    | ((getHand game hand1) == Straight) && (elem (getHand game hand2) (enumFromTo HighestCard ThreeCards)) = GT
    | ((getHand game hand1) == Flush) && (elem (getHand game hand2) (enumFrom FullHouse)) = LT
    | ((getHand game hand1) == Flush) && (elem (getHand game hand2) (enumFromTo HighestCard Straight)) = GT
    | ((getHand game hand1) == FullHouse) && (elem (getHand game hand2) (enumFrom FourCards)) = LT
    | ((getHand game hand1) == FullHouse) && (elem (getHand game hand2) (enumFromTo HighestCard Flush)) = GT
    | ((getHand game hand1) == FourCards) && ((getHand game hand2) == StraightFlush) = LT
    | ((getHand game hand1) == FourCards) && (elem (getHand game hand2) (enumFromTo HighestCard FullHouse)) = GT
    | ((getHand game hand1) == StraightFlush) && not ((getHand game hand2) == StraightFlush) = GT
    | otherwise  = EQ
