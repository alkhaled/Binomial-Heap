module BinomHeap where


-- Binomial Heap Definition
data BinHeap tree val =   Hole (BinHeap (ChildList tree) val)
                        | Full (BinTree tree val) (BinHeap (ChildList tree) val)
                        | End deriving (Eq, Show)

--TODO: Complete This Implementation
merge :: Ord val => BinHeap tree val ->  BinHeap tree val-> BinHeap tree val
merge binHeap1 binHeap2 = case (binHeap1, binHeap2) of
                               (End, _) -> binHeap2
                               (_, End) -> binHeap1
                               (Hole rest1, Hole rest2) -> Hole (merge rest1 rest2)
                               (Hole rest1, Full tree rest2) -> Full tree (merge rest1 rest2)
                               (Full tree rest1,  Hole rest2) -> Full tree (merge rest1 rest2)

  
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

--Heap Tests

--Test merging two heaps with no trees of the same rank
--    111   Rank3 All Full
-- + 1000   Rank4 Last Full
--  ------
--   1111   Rank4 All Full
test_Merge_No_Carry = (merge heap3Full heap4End) == heap4Full
            where
                heap3Full = Full (rank1) (Full rank2 (Full rank3 (End)))
                heap4End  = Hole (Hole (Hole (Full rank4 End)))
                heap4Full = Full (rank1) (Full rank2 (Full rank3 (Full rank4 End)))



-- | The main entry point.
main :: IO ()
main = do
    putStrLn $ show $ test_Merge_No_Carry
