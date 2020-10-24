assignToGroups people groups = zip people (cycle groups)

-------------------------------------------------------

repeat2 n = cycle [n]

-------------------------------------------------------

subseq start end list = take diff (drop start list)
  where diff = end - start

-------------------------------------------------------

inFirstHalf item list = elem item firstHalf
  where firstHalf = take midpoint list
        midpoint = (length list) `div` 2