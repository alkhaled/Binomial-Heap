module BinomHeapTests where
import BinomHeap


-- Trees used in tests set all values to one for easy equality tests
testrank1 =  BinTree 1 Empty
testrank2 =  BinTree 1 (ChildList (BinTree 1 Empty) Empty)
testrank3 =  BinTree 1 (ChildList (BinTree 1 (ChildList (BinTree 1 Empty) Empty)) (ChildList (BinTree 1 Empty) Empty))
testrank4 =  BinTree 1 (ChildList
                            (BinTree 1 (ChildList (
                                                    BinTree 1 
                                                    (ChildList (BinTree 1 Empty) Empty)) (ChildList (BinTree 1 Empty) Empty))) 
                            (ChildList (BinTree 1 (
                                                    ChildList (BinTree 1 Empty) Empty)) (ChildList (BinTree 1 Empty) Empty)))

                                                    
--Heap Tests


--Test merging two heaps with no trees of the same rank
--    111   Rank3 All Full
-- + 1000   Rank4 Last Full
--  ------
--   1111   Rank4 All Full

test_Merge_No_Carry = (merge heap3Full heap4End (<)) == heap4Full
            where
                heap3Full = Full (rank1) (Full rank2 (Full rank3 (End)))
                heap4End  = Hole (Hole (Hole (Full rank4 End)))
                heap4Full = Full (rank1) (Full rank2 (Full rank3 (Full rank4 End)))



--Test merging two heaps with no trees of the same rank
--   0111   Rank3 All Full
-- + 0001  
--  ------
--   1000   Rank4 Last Full

test_Merge_With_Carry = (merge heap3Full heap1Full (<)) == heap4End
                    where 
                        heap3Full = Full (testrank1) (Full testrank2 (Full testrank3 (End)))
                        heap1Full = Full (testrank1) End
                        heap4End  = Hole (Hole (Hole (Full testrank4 End)))



-- | The main entry point.
main :: IO ()
main = do
    putStrLn $ show $ test_Merge_With_Carry