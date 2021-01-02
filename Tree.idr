module Main

data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)

insert : Ord elem =>
         elem ->
         Tree elem ->
         Tree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left focus right)
  = case x `compare` focus of
         LT => Node (insert x left) focus right
         GT => Node left focus (insert x right)
         EQ => orig

listToTree : Ord a => List a -> Tree a
listToTree = foldr insert Empty

treeToList : Ord a => Tree a -> List a
treeToList Empty = []
treeToList (Node left focus right) = treeToList left ++ focus :: treeToList right
