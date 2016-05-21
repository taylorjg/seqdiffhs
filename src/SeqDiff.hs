module SeqDiff (
    diffs,
    DiffType,
    Diff
) where

data DiffType = Duplicate | Missing deriving Show
data Diff = Diff { diffType :: DiffType, value :: Int, count :: Int } deriving Show

diffs :: [Int] -> [Diff]
diffs xs = loop xs expectedValue repeatCount
    where
        expectedValue = 0
        repeatCount = 0

loop :: [Int] -> Int -> Int -> [Diff]
loop xs ev rc =
    case xs of
        v:vs | v == ev - 1 -> loop vs ev (rc + 1)
        v:vs ->
            let
                d1 = if (rc > 0) then Just (Diff {diffType = Duplicate, value = ev - 1, count = rc}) else Nothing
                d2 = if (v > ev) then Just (Diff {diffType = Missing, value = ev, count = v - ev}) else Nothing
                ds = loop vs ev' rc'
                ev' = v + 1
                rc' = 0
            in
                case (d1, d2) of
                    (Just d1, Just d2) -> d1 : d2 : ds
                    (Just d1, Nothing) -> d1 : ds
                    (Nothing, Just d2) -> d2 : ds
                    (Nothing, Nothing) -> ds
        _ ->
            let
                d = if (rc > 0) then Just (Diff {diffType = Duplicate, value = ev - 1, count = rc}) else Nothing
            in
                case d of
                    Just d -> [d]
                    Nothing -> []
