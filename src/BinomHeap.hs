module BinomHeap where


-- Binomial Heap Definition
data BinHeap tree val =   Hole (BinHeap (ChildList tree) val)
                        | Full (BinTree tree val) (BinHeap (ChildList tree) val)
                        | End deriving (Eq, Show)

-- Note: the carry in this merge method is horribly inefficient in the case (Full, Full). We should really use a carry to make the algorithm linear, but this simpler implementation
-- makes it easier to explain the algorithm and understand the role of type level guarantees.
merge :: Ord val => BinHeap tree val ->  BinHeap tree val-> (val -> val -> Bool) ->BinHeap tree val
merge binHeap1 binHeap2 cmp = case (binHeap1, binHeap2) of
                               (End, _) -> binHeap2
                               (_, End) -> binHeap1
                               (Hole rest1, Hole rest2) -> Hole (merge rest1 rest2 cmp)
                               (Hole rest1, Full tree rest2) -> Full tree (merge rest1 rest2 cmp)
                               (Full tree rest1,  Hole rest2) -> Full tree (merge rest1 rest2 cmp)
                               (Full tree1 rest1, Full tree2 rest2) -> Hole (merge (merge (Full (meld tree1 tree2 cmp) End) rest1 cmp) 
                                                                                   rest2 cmp) 


 
-- Binomial Tree Definition
data Empty a = Empty deriving (Eq,Show)
data BinTree child val = BinTree val (child val)  deriving (Eq,Show)
data ChildList child val = ChildList (BinTree child val) (child val) deriving (Eq, Show)

meld :: (Ord val) => BinTree child val -> BinTree child val -> (val -> val -> Bool) -> BinTree (ChildList child) val
meld bt1@(BinTree val1 kids1) bt2@(BinTree val2 kids2) cmp | val1 `cmp` val2 = BinTree val1 (ChildList bt2 kids1)
                                                           | otherwise = BinTree val2 (ChildList bt1 kids2)


-- # Example Binomial Trees
rank1 =  BinTree 1 Empty
rank2 =  BinTree 2 (ChildList (BinTree 1 Empty) Empty)
rank3 =  BinTree 3 (ChildList (BinTree 2 (ChildList (BinTree 1 Empty) Empty)) (ChildList (BinTree 1 Empty) Empty))
rank4 =  BinTree 4 (ChildList
                            (BinTree 3 (ChildList (
                                                    BinTree 2 
                                                    (ChildList (BinTree 1 Empty) Empty)) (ChildList (BinTree 1 Empty) Empty))) 
                            (ChildList (BinTree 2 (
                                                    ChildList (BinTree 1 Empty) Empty)) (ChildList (BinTree 1 Empty) Empty)))

rank5 = BinTree 5 (ChildList
                        (BinTree 4 (ChildList 
                                            (BinTree 3 (ChildList (BinTree 2 (ChildList (BinTree 1 Empty) Empty)) (ChildList (BinTree 1 Empty) Empty))) 
                                            (ChildList (BinTree 2 (ChildList (BinTree 1 Empty) Empty)) (ChildList (BinTree 1 Empty) Empty))))
                        (ChildList
                            (BinTree 3 (ChildList (
                                                    BinTree 2
                                                    (ChildList (BinTree 1 Empty) Empty)) (ChildList (BinTree 1 Empty) Empty))) 
                            (ChildList (BinTree 2 (
                                                    ChildList (BinTree 1 Empty) Empty)) (ChildList (BinTree 1 Empty) Empty))))


rank6 = BinTree 6 (ChildList rank5 (ChildList rank4 (ChildList rank3 (ChildList rank2 (ChildList (BinTree 0 Empty) Empty)))))
rank7 = BinTree 7 (ChildList rank6 (ChildList rank5 (ChildList rank4 (ChildList rank3 (ChildList rank2 (ChildList (BinTree 0 Empty) Empty))))))

-- # End Examples



-- | The main entry point.
main :: IO ()
main = do
    putStrLn $ show $ rank7
