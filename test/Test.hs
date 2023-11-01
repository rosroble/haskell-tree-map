import Test.HUnit(assertEqual, runTestTTAndExit, Test(..))

import TreeMap
    (Tree, put, get, size, removeKey, containsKey, mapFromList, filterMap, foldlMap, foldrMap, mapMap, mergeMaps)

main :: IO ()
main = do
    runTestTTAndExit tests

tests :: Test
tests = TestList [
    TestLabel "test basic map operations" testBaseOperations,
    TestLabel "test filter" testFilter,
    TestLabel "test fold" testFold,
    TestLabel "test map" testMap,
    TestLabel "test merge" testMerge
    ]

testBaseOperations :: Test
testBaseOperations = TestCase ( do
    let treeMap = mapFromList [(4, "four"), (3, "thr33"), (11, "eleven"), (9, "nine"), (100, "hundred"), (3, "three")]
    assertEqual "Test assert size" 5 (TreeMap.size treeMap)
    assertEqual "Test assert get 4" (Just "four") (TreeMap.get 4 treeMap)
    assertEqual "Test assert get 3" (Just "thr33") (TreeMap.get 3 treeMap) 
    assertEqual "Test assert contains 11" True (TreeMap.containsKey 11 treeMap) 
    assertEqual "Test assert does not contain 8" False (TreeMap.containsKey 8 treeMap) 

    let afterPut = TreeMap.put 8 "eight" treeMap
    assertEqual "Test assert size changed" 6 (TreeMap.size afterPut) 
    assertEqual "Test assert new key is present" True (TreeMap.containsKey 8 afterPut) 
    assertEqual "Test assert new key value is correct" (Just "eight") (TreeMap.get 8 afterPut) 

    let afterRemove = TreeMap.removeKey 11 afterPut
    assertEqual "Test assert size changed" 5 (TreeMap.size afterRemove) 
    assertEqual "Test assert removed key is absent" False (TreeMap.containsKey 11 afterRemove)
    assertEqual "Test assert removed key is absent" Nothing (TreeMap.get 11 afterRemove) 
    )

testFilter :: Test
testFilter = TestCase ( do
    let treeMap = filterMap (> 9) $ mapFromList [(4, "four"), (3, "thr33"), (11, "eleven"), (9, "nine"), (100, "hundred"), (3, "three")]
    assertEqual "Test assert size" 2 (TreeMap.size treeMap)    
    )


testFold :: Test
testFold = TestCase ( do
    let tree = mapFromList [(4, "four"), (11, "eleven"), (9, "nine"), (100, "hundred"), (3, "three")]
    let sumKeysLeft = foldlMap (\cnt (k,_) -> cnt + k) 0 tree
    assertEqual "Test assert sum keys left" 127 sumKeysLeft
    let sumKeysRight = foldrMap ((+) . fst) 0 tree
    assertEqual "Test assert sum keys" 127 sumKeysRight
    )


testMap :: Test
testMap = TestCase ( do
    let treeBefore = mapFromList [(1 :: Integer, 2 :: Integer), (3 :: Integer, 4 :: Integer), (5 :: Integer, 6 :: Integer)]
    let treeAfterExpected = mapFromList [(6, 12), (4, 8), (2, 4)]
    let treeAfterActual = mapMap (\(k, v) -> (k + 1, v * 2)) treeBefore
    assertEqual "Test assert size has not changed" (TreeMap.size treeBefore) (TreeMap.size treeAfterActual)
    assertEqual "Test assert mapped tree equals expected" treeAfterExpected treeAfterActual
    )


testMerge :: Test
testMerge = TestCase ( do
    let firstTree = mapFromList [(4, "four"), (11, "eleven"), (9, "nine"), (100, "hundred"), (3, "three")]
    let secondTree = mapFromList[(11, "eleven"), (10, "ten"), (100, "hundred"), (88, "88")]
    let merged = mergeMaps firstTree secondTree
    assertEqual "Test assert size is correct" 7 (TreeMap.size merged)
    )