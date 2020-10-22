module Day7 where
    --thank you ormolu for styling tips. 
    --now the syntax structure aids comprehension, like it 
    --was originally intended

    
    data Point --type
        = Point3d Float Float Float --constructor variant: (Float -> Float -> Float) -> Type
        | Point2d Float Float   --constructor variant: Float -> Float -> Type
        deriving (Show)

    getX (Point2d x _) = x
    getX (Point3d x _ _) = x

    --multiple definitions need to be grouped
    getY (Point2d _ y) = y     
    
    getY (Point3d _ y _) = y 
    --   ^--- notice pattern matching and the substitution at play

    getZ (Point3d _ _ z) = z

    data Shape 
        = Rectangle Point Point
        | Circle Point Float
        | Triangle Point Point Point
        deriving (Show)
    
    data ConicShape = Conic Shape Point

    -- this type has two constructors: 
    -- one is recursive, and the other is terminal. 
    data List elm   
        =   Cons' elm (List elm) 
        |   Nil'  
        deriving (Show)
    
    cons_list = Cons' 3 (Cons' 2 (Cons' 1 Nil'))

    -- an n-tree.
    data Tree leaf = Node leaf [Tree leaf] deriving (Show)

    kinship_tree = Node "Father" [Node "Daughter" [], Node "Son" [], Node "Donkey" []]

    data BinaryTree leafKind
        = Empty
        | BNode leafKind (BinaryTree leafKind) (BinaryTree leafKind)

    left_tree = BNode 5 (BNode 4 Empty Empty) (BNode 6 Empty Empty)
    right_tree = BNode 15 (BNode 10 Empty Empty) (BNode 20 Empty Empty)

    whole_tree = BNode 10 left_tree right_tree

    preorder :: BinaryTree kind -> [kind]
    preorder Empty = [] --subsitute Empty into []
    preorder (BNode value left_tree right_tree) = [value] ++ preorder left_tree ++ preorder right_tree

    inorder :: BinaryTree kind -> [kind]
    inorder Empty = []
    inorder (BNode value left right) = inorder left ++ [value] ++ inorder right

    postorder :: BinaryTree kind -> [kind]
    postorder Empty = []
    postorder (BNode value left right) = postorder left ++ postorder right ++ [value]

    postorderl :: BinaryTree kind -> [kind]
    postorderl Empty = []
    postorderl (BNode value left right) = postorder right ++ postorder left ++ [value]

