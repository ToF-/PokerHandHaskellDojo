# PokerHandHaskellDojo

Write a program that when given a list of hands in the game of Texas Hold'em Poker, will display the category of hand, and which hand is winning.

Each line of the entry file represent one of the player's carlds. Each card is a pair of characters, the first character represents the face, the second is the suit. Cards are separated by exactly one space.

The face values for 10, Jack, Queen, King and Ace are T,J,Q,K and A respectively.
The suit values for Clubs, Diamonds, Hearts and Spades are c,d,h and s respectively.

If a player folds, only his hole cards and the community cards are shown.

## Hand Value

Following table shows the possible hand values in increasing order.

If the hand involves fewer than five cards, (such as two pair or three of a kind), then kickers are used to settle ties (see the second example below). The card's numerical rank is of sole importance; suit values are irrelevant in hold 'em.

    Name            Description                                                                     Example
    Highcard        Simple value of the card. Lowest: 2 – Highest: Ace (King in example)            Tc 4h 7d Kc 2s
    Pair            Two cards with the same value                                                   Kc Kh 7d 2c 4s
    Two pairs       Two times two cards with the same value                                         Kc Kh 7d 7c 5s
    Three of a kind Three cards with the same value                                                 Kc Kh Kd 7c 5s
    Straight        Sequence of 5 cards in increasing value (Ace can precede 2 and follow up King)  3c 4h 5d 6c 7s
    Flush           5 cards of the same suit                                                        Kc Qc 9c 8c 2c
    Full house      Combination of three of a kind and a pair                                       Kc Kh Kd 7c 7s
    Four of a kind  Four cards of the same value                                                    6s 6d 6h 6c Ks
    Straight flush  Straight of the same suit                                                       2s 3s 4s 5s 6s
    Royal flush     Straight flush from Ten to Ace                                                  Ts Js Qs Ks As

## Input Example

    Kc 9s Ks Kd 9d 3c 6d
    9c Ah Ks Kd 9d 3c 6d
    Ac Qc Ks Kd 9d 3c
    9h 5s
    4d 2d Ks Kd 9d 3c 6d
    7s Ts Ks Kd 9d


## Output Example

    Kc 9s Ks Kd 9d 3c 6d Full House (winner)
    9c Ah Ks Kd 9d 3c 6d Two Pair
    Ac Qc Ks Kd 9d 3c
    9h 5s
    4d 2d Ks Kd 9d 3c 6d Flush
    7s Ts Ks Kd 9d

## TODO
- Trouver la valeur des cartes
- Comparer deux chaines de caractères qui représentent une carte
- Faire un appel de fonction dans le test

Comparer 2 cartes unitairement :
Comparer 2 mains -> Pour comparer 2 mains, il faut savoir donner la meilleure catégorie 
Il faut aussi savoir comparer des combinaisons

