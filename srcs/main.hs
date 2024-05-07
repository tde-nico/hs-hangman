import GHC.IO.Handle (hSetEcho)
import GHC.IO.StdHandles (stdin)

secretGetChar = do
	hSetEcho stdin False
	x <- getChar
	hSetEcho stdin True
	return x

secretGetLine :: IO [Char]
secretGetLine = do
	x <- secretGetChar
	if x == '\n' then do
		putChar x
		return []
	else do
		putChar '-'
		xs <- secretGetLine
		return (x:xs)

match xs ys = [if x `elem` ys then x else '-' | x <- xs]

play word = do
	putStr "?"
	guess <- getLine
	if guess == word then
		putStrLn "You got it!!"
	else do
		putStrLn (match word guess)
		play word

hangman :: IO()
hangman = do
	putStrLn "Think a word:"
	word <- secretGetLine
	putStrLn "Try to guess it:"
	play word

main = do
	hangman

