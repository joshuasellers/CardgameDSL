# CardGameDSL

###### Goal:
I want to be able to create the logic and syntax that can be used to create the rules for card games in the form of a Domain Specific Language.  

  - Domain is any card game involving a regular playing card deck
  - Initial work done over the course of two weeks
  - Room still left for future improvements

### Overall Usability

I envision this being used to create any kind of game that requires one or more regular playing card decks (so not something like Uno).  Essentially all the rules and scoring must involve the cards from the deck (this excludes games like Spoons where you have actual spoons in play).

> The first iteration has enough easy usability
> for games like Texas Hold'Em and Five Card Draw.
> It can do more, but that involves more practice
> and refinement with the language.


### Description
This DSL was written in Haskell.  I chose the language due to my familiarity, I used it in a prior course, and its flexibiliy (you can do quite a bit with it ... if you are able to find and understand the information; the Wikis are a bit dense).  I especially used Haskells' `data` and `class` tools in Haskell to great effect.

The DSL is comprised of seven `data` types.  They are housed in `Types.hs`.

| Type | Description |
| ------ | ------ |
| Rank | e.g. King or Queen |
| Suit | e.g. Spade or Clubs |
| Card | Each Card is a combination of a Rank and a Suit |
| Player | Has things like Hand and Score associated with it |
| Dealer | Essentially a Player with access to the deck-functions |
| Table | For games like Texas Hold'Em |
| Game | Has all of the programmer-defined rules |

These datatypes are used throughout the DSL and are the buildingblocks that go into almost every function.  Here is the code for `Card`:

```sh
data Card = Card {_rank :: Rank, _suit :: Suit} 
 deriving (Eq)
instance Show Card where
    show (Card r s) = show r ++ show s
```

There are two other important files: `Game.hs` and `Functions.hs` (I also made example files, but they are not relavent to the DSL itself).  `Function.hs` contains all fo the general functions for the DSL.  Examples include `draw` and `deal`:

```sh
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
```
The `class` allowed for polymorphism when it comes to commonly used functions like `draw` and `deal`.  I had to include these tags to make them work:

```sh
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
```
For `Game.hs`, I put all of the rule-specific functions into there.  The most important functions, that are also included in the `Game` datatype, are involved with comparing hands and scoring.  I made the decision to have the user actually define those areas, to allow for more flexibility, but I did write numerous helper functions for the user to utilize like these:
```sh
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
```
### Example Code
I made an example to show some basic usage of the DSL.  I created a simplified version of Five Card Draw Poker.  I reused my compare method from a previous project that specifically focussed on Poker and Cribbage.  The files for this example are: `ExampleMain.hs` and `PokerExample.hs`.  I made a simple `main` to run a game.
### Next Steps
- Update discard and fold to not return a tuple (I already fixed that for deal)
- Add betting functions and fields for players and dealers
- Add in odds-calculating functions so that programmers can encode a computer oponent
- Test out some non-traditional cards games (like War or Go Fish)
- Clean up some functions that are outdated after I improved other, similar, ones
- Add in more helper functions to `Game.hs` - `PokerExample.hs` still wound up being a fairly large file