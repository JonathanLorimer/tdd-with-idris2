module Main

data BSTree : Type -> Type where
  Empty : Ord elem => BSTree elem
  Node : Ord elem => (left : BSTree elem) -> (val : elem) ->
                     (right : BSTree elem) -> BSTree elem

insert : elem ->
         BSTree elem ->
         BSTree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left focus right)
  = case x `compare` focus of
         LT => Node (insert x left) focus right
         GT => Node left focus (insert x right)
         EQ => orig
