module SeqDiff (
    diffs,
    DiffType,
    Diff
) where

data DiffType = Duplicate | Missing deriving Show
data Diff = Diff { diffType :: DiffType, value :: Int, count :: Int } deriving Show

diffs :: [Int] -> [Diff]
diffs xs = loop xs 0 0

loop :: [Int] -> Int -> Int -> [Diff]
loop xs ev rc =
    case xs of
        v:vs | v == (pred ev) ->
            loop vs ev (succ rc)
        v:vs ->
            let
                d1 = if (rc > 0) then Just $ makeDuplicate (pred ev) rc else Nothing
                d2 = if (v > ev) then Just $ makeMissing ev (v - ev) else Nothing
                ds = loop vs (succ v) 0
            in
                case (d1, d2) of
                    (Just d1, Just d2) -> d1 : d2 : ds
                    (Just d1, Nothing) -> d1 : ds
                    (Nothing, Just d2) -> d2 : ds
                    (Nothing, Nothing) -> ds
        [] ->
            if (rc > 0) then return $ makeDuplicate (pred ev) rc else []

makeDuplicate :: Int -> Int -> Diff
makeDuplicate v c = Diff {diffType = Duplicate, value = v, count = c}

makeMissing :: Int -> Int -> Diff
makeMissing v c = Diff {diffType = Missing, value = v, count = c}
