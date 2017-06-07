module Cards

type Suit = 
    | Hearts
    | Diamonds
    | Clubs
    | Spades

type CardRank = 
    | Ace
    | King
    | Queen
    | Jack
    | Ten
    | Nine
    | Eight
    | Seven
    | Six
    | Five
    | Four
    | Three
    | Two

type Card = CardRank * Suit

let Deck : Card list =
    let suits =  [ Hearts; Diamonds; Clubs; Spades ]
    let ranks = [ Ace; King; Queen; Jack; Ten; Nine; Eight; Seven; Six; Five; Four; Three; Two ]
    [ for s in suits do
        for r in ranks do
            yield (r, s) ]