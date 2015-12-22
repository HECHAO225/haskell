data Season = Spring | Summer | Automn | Winter
     deriving (Show, Eq, Enum, Ord, Read)
     
data Expr a= Lit a
    | (Expr a) :+: (Expr a)
    | (Expr a) :-: (Expr a) deriving (Show)
