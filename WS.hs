main = interact wordCount
         where wordCount input = show (map length (lines input)) ++ "\n"
