{-
    solutions can be find by heuristic algorithm. However it won't always find the best solution, what's
    more, it can't find all solutions.
    To find all solutions, I perform a dfs algorithm to find all combinations of the members, this
    is really slow. To make it faster, I cut all leaves that are useless using the check_differ
    and check_familiar conditions.
-}
module Task1 where

{-
    Bids is a matrix.
    the height is number of patent requests (n),
    the width is number of technical member (n).

-}
type Bids = [[Int]]

{-
    Save Result, the Result is a list with length n
-}
type Result = [Int]

{-
    Save Results, the result is a matrix with length n
-}
type Results = [[Int]]

{-
    Find all valid solution
    0 - INEXPERT
    1 - FAMILIAR
    2 - KNOWLEDGEABLE
    3 - EXPERT
    *Task1> bids = [[0, 1, 2, 3, 2], [0, 1, 2, 3, 2], [3, 1, 2, 0, 2], [0, 3, 2, 1, 2], [0, 1, 3, 3, 2]]
    The number of patent is from 0 to n - 1
    the index represents patent, the content of bids[index] is the level of expertise of each member.

    *Task1> n = 5
    *Task1> m = 3
    *Task1> k = 2
    *Task1> best bids n m k
    [[3,3,0,1,3],[3,3,0,1,2]]
    the result is a matrix, each line represents one solution,
    the [3, 3, 0, 1, 3] means patent of index 0 is reviewed by the third member,
        patent of index 2 is reviewed by the first member, and so on.
-}
best :: Bids -> Int -> Int -> Int -> Results
best bids n m k = best_solution
  where
    results = get_results bids k m -- get all possible result
    max_s = max_score bids results -- find the max score
    best_solution = get_best_solution bids results max_s -- get best solution

-- Helper functions --
{-
    Get best solutions
-}
get_best_solution :: Bids -> Results -> Int -> Results
get_best_solution bids results s = get_min_results bids results s

{-
    Get all results that has a score of the given score
-}
get_min_results :: Bids -> Results -> Int -> Results
get_min_results bids [] _ = []
get_min_results bids (res:xs) s
  | score bids res == s = (get_min_results bids xs s) ++ [res]
  | otherwise = get_min_results bids xs s

{-
    Get all possible solutions
-}
get_results :: Bids -> Int -> Int -> Results
get_results bids k m = get_result bids [] k m 0

{-
    Get all possible solutions
    Use a dfs algorithm to generate all combinations, to make it faster,
    cut useless leaves using the check_differ and check_familiar.
-}
get_result :: Bids -> Result -> Int -> Int -> Int -> Results
get_result bids res k m i
  | i == length bids && check_differ res m && check_familiar bids res k 0 =
    [res] -- cut leaves that are useless
  | i >= length bids = []
  | otherwise = (dfs bids (bids !! i) res k m 0 i)

{-
    Dfs algorithm
-}
dfs :: Bids -> [Int] -> Result -> Int -> Int -> Int -> Int -> Results
dfs bids bid res k m n i
  | n >= length bid = []
  | (bid !! n) /= 0 =
    (get_result bids (res ++ [n]) k m (i + 1)) ++
    (dfs bids bid res k m (n + 1) i)
  | otherwise = (dfs bids bid res k m (n + 1) i)

{-
    Find max score of all solutions
-}
max_score :: Bids -> Results -> Int
max_score bids res = _maxScore bids res 0

{-
    Find max score of all solutions
-}
_maxScore :: Bids -> Results -> Int -> Int
_maxScore _ [] c = c
_maxScore bids (res:xs) c
  | co > c = _maxScore bids xs co
  | otherwise = _maxScore bids xs c
  where
    co = score bids res

{-
    Find score of one solution
-}
score :: Bids -> Result -> Int
score [] [] = 0
score (bx:bxs) (br:brs)
  | c == 1 = 1 + score bxs brs
  | c == 2 = 4 + score bxs brs
  | c == 3 = 10 + score bxs brs
  where
    c = bx !! br

{-
    Check the differ, members' job should not differ by k
-}
check_differ :: Result -> Int -> Bool
check_differ res m
  | (min_list list) + m < max_list list = False
  | otherwise = True
  where
    list = list_count res 0

{-
    Check the familiar, members' familiar job should not be over m
-}
check_familiar :: Bids -> Result -> Int -> Int -> Bool
check_familiar bids res k i
  | i == length bids = True
  | count_familiar bids res i > k = False
  | otherwise = check_familiar bids res k (i + 1)

{-
    Count the familiar job
-}
count_familiar :: Bids -> Result -> Int -> Int
count_familiar [] [] _ = 0
count_familiar (bx:bxs) (br:brs) i
  | bx !! br == 1 && br == i = 1 + count_familiar bxs brs i
  | otherwise = count_familiar bxs brs i

{-
    Count the jobs of all members
-}
list_count :: Result -> Int -> [Int]
list_count res i
  | i == length res = []
  | otherwise = [count_member res i] ++ (list_count res (i + 1))

{-
    Count the jobs of one member
-}
count_member :: Result -> Int -> Int
count_member [] _ = 0
count_member (x:xs) i
  | x == i = 1 + count_member xs i
  | otherwise = count_member xs i

{-
    Find the minimum element of the list
-}
min_list :: Result -> Int
min_list [] = 100000
min_list (x:xs)
  | x < min_x = x
  | otherwise = min_x
  where
    min_x = min_list xs

{-
    Find the max element of the list
-}
max_list :: Result -> Int
max_list [] = 0
max_list (x:xs)
  | x > max_x = x
  | otherwise = max_x
  where
    max_x = max_list xs
