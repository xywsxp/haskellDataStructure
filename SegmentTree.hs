data SegmentTree a = Nil | Node a (SegmentTree a) (SegmentTree a) deriving (Show)

takeValue :: (SegmentTree a) -> a
takeValue (Node v _ _) = v

buildTree :: (Integral t) => t -> t -> a -> (a->a->a) -> SegmentTree a
buildTree l r val f 
    | l == r = Node val Nil Nil
    | otherwise = Node newVal left right
    where 
        mid = div (r-l+1) 2 + l - 1
        left = buildTree l mid val f
        right = buildTree (mid+1) r val f
        newVal = f (takeValue left) (takeValue right)

modifyTree :: (Integral t) => SegmentTree a -> t -> t -> t -> a -> (a->a->a) -> SegmentTree a
modifyTree (Node val left right) l r pos v f 
    | pos > r || pos < l = Node val left right
    | l == r = Node v Nil Nil
    | otherwise = Node newVal newLeft newRight
    where 
        mid = div (r-l+1) 2 + l - 1
        newLeft = modifyTree left l mid pos v f
        newRight = modifyTree right (mid+1) r pos v f
        newVal = f (takeValue newLeft) (takeValue newRight)

queryTree :: (Integral t) => SegmentTree a -> t -> t -> t -> t -> (a->a->a) -> a
queryTree (Node val left right) l r ql qr f 
    | ql <= l && qr >= r = val
    | mid >= ql = queryTree left l mid ql qr f
    | mid < qr = queryTree right (mid+1) r ql qr f
    | otherwise = f (queryTree left l mid ql qr f) (queryTree right (mid+1) r ql qr f)
    where mid = div (r-l+1) 2 + l - 1