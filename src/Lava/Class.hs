
module Lava.Class where


class ArrowLava l where 
  (>|>) :: l a b -> l b c -> l a c
  (->-) :: l a b -> l b c -> l a c
infixr 4 >|>, <|<
infixr 1 ->-, -<-

(-<-) :: ArrowLava l => l b c -> l a b -> l a c
(-<-) = flip (->-)

(<|<) :: ArrowLava l => l b c -> l a b -> l a c
(<|<) = flip (>|>)
