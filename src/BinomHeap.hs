module BinomHeap where




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
    putStrLn "Binomial Tree rank 7: "
    putStrLn $ show $ (meld rank6 rank6 (>))
  
