import Data.List
import System.IO

data LineOfText = LineOfText([Char], [Char], [Char], [Char]) deriving (Show)

a :: LineOfText
a = LineOfText("This is a", "", "1234", " ooooo ")

cursorLeft :: LineOfText -> LineOfText
cursorLeft (LineOfText(l, s, r, b)) = (LineOfText((reverse (tail (reverse (l)))), (s), ((head (reverse (l))) : r), (b)))

cursorRight :: LineOfText -> LineOfText
cursorRight (LineOfText(l, s, r, b)) = (LineOfText((l ++ [(head(r))]), (s), (tail(r)), (b)))

cursorStart :: LineOfText -> LineOfText
cursorStart (LineOfText(l, s, r, b)) = (LineOfText((l ++ r), (s), ([]), (b)))

deleteLeft :: LineOfText -> LineOfText
deleteLeft (LineOfText(l, s, r, b)) = (LineOfText((reverse (tail (reverse (l)))), (s), (r), (b)))

deleteRight :: LineOfText -> LineOfText
deleteRight (LineOfText(l, s, r, b)) = (LineOfText((l), (s), (tail(r)), (b)))

cursorStartWord :: LineOfText -> LineOfText
cursorStartWord (LineOfText(l, s, r, b)) 
    | [(head (reverse (l)))] /= " " = cursorStartWord(cursorLeft (LineOfText(l, s, r, b)))
    | otherwise = (LineOfText((l), (s), (r), (b))) 

cursorEndWord :: LineOfText -> LineOfText
cursorEndWord (LineOfText(l, s, r, b)) 
    | [(head (r))] /= " " = cursorEndWord(cursorRight (LineOfText(l, s, r, b)))
    | otherwise = (LineOfText((l), (s), (r), (b))) 

deleteWord :: LineOfText -> LineOfText
deleteWord (LineOfText(l, s, r, b)) 
    | [(head (reverse (l)))] /= " " = deleteWord(deleteLeft (LineOfText(l, s, r, b)))
    | [(head (r))] /= " " = deleteWord(deleteRight (LineOfText(l, s, r, b)))
    | otherwise = (LineOfText((l), (s), (tail(r)), (b))) 

highlightCharLeft :: LineOfText -> LineOfText
highlightCharLeft (LineOfText(l, s, r, b)) = (LineOfText((reverse (tail (reverse (l)))), (head (reverse (l)) : s), (r), (b)))

highlightCharRight :: LineOfText -> LineOfText
highlightCharRight (LineOfText(l, s, r, b)) = (LineOfText((l), ([head(r)]), (s ++ tail(r)), (b)))

highlightWordLeft :: LineOfText -> LineOfText
highlightWordLeft (LineOfText(l, s, r, b)) 
    | [head (reverse (l))] == " " = (LineOfText((l), (s), (r), (b))) 
    | otherwise = highlightWordLeft(highlightCharLeft (LineOfText(l, s, r, b)))
    
highlightAllLeft :: LineOfText -> LineOfText
highlightAllLeft (LineOfText(l, s, r, b)) = (LineOfText(([]), (l ++ s), (r), (b))) 
    
highlightAllRight :: LineOfText -> LineOfText
highlightAllRight (LineOfText(l, s, r, b)) = (LineOfText((r), (s ++ l), ([]), (b))) 
    
copy :: LineOfText -> LineOfText
copy (LineOfText(l, s, r, b)) = (LineOfText((l), (s), (r), (s))) 
    
cut :: LineOfText -> LineOfText
cut (LineOfText(l, s, r, b)) = (LineOfText((l), ([]), (r), (s))) 
    
paste :: LineOfText -> LineOfText
paste (LineOfText(l, s, r, b)) = (LineOfText((l), ([]), (b ++ r), (b))) 
    
clearPaste :: LineOfText -> LineOfText
clearPaste (LineOfText(l, s, r, b)) = (LineOfText((l), (s), (r), ([])))  

readFromFile = do

    theFile2 <- openFile "test.txt" ReadMode
    
    ReadText <- hGetContents theFile2
    putStr readText
    
    hClose theFile2

putInvariable :: LineOfText -> LineOfText
putInvariable (LineOfText(l, s, r, b)) = (LineOfText((), (s), (), (b))) 



    
highlightWordRight :: LineOfText -> LineOfText
highlightWordRight (LineOfText(l, s, r, b)) 
    | [head(r)] == " " = (LineOfText((l), (s), (r), (b))) 
    | otherwise = highlightWordRight(highlightCharRight (LineOfText(l, s, r, b)))


 -- (LineOfText(l, s, r, b)) = (LineOfText((), (s), (), (b))) 