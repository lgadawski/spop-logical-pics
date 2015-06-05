import Data.Matrix
import qualified Data.Vector

main = do
	print (findAllHash 
			[1, 4, 3, 2]
			[2, 3, 2, 3])
			--[[1], [4], [3], [2]]
			--[[2], [3], [2], [3]])
		--[[3], [2, 2], [2, 2], [3], [1], [1], [1], [1], [2], [3]] 
		--[[2, 1], [4, 2], [1, 7], [4], [2]]

--[1, 0, 2, 1, 2, 1] [1, 1, 2, 1, 1, 1] [(0, 1), (3, 2), (3, 4), (4, 0), (4, 4), (5, 2), (5, 5)] )

-- Funkcja inicująca metodę znajdująca wszystkie zamalowane punkty
findAllHash ys xs =  findAllHash' ys xs (matrix (length ys) (length xs) $ \(_,_) -> 0)

findAllHash' :: (Num m) => [Int] -> [Int] -> Matrix m -> Matrix m
findAllHash' ys xs result | isAllBoardFilled ys xs result = result
						  | otherwise = findAllHash' ys xs (fillAllRows ys (fillAllCols xs result))
--findAllHash' ys xs result = findAllHash' ys xs (fillAllRows ys (fillAllCols xs result))

-- funkcja sprawdzająca czy cała plansza jest wypełniona
isAllBoardFilled :: (Num m) => [Int] -> [Int] -> Matrix m -> Bool
isAllBoardFilled _ = True

-- funkcja wypelniajaca linie w calosci w przypadku gdy liczba pozycji do zamalowania rowna 
-- jest dlugosci wiersza
-- mapRow (Map a function over a row)
fillAllRows :: (Num m) => [Int] -> Matrix m -> Matrix m
fillAllRows [] result = result
fillAllRows (y:ys) result | y == (Data.Vector.length (getRow y result)) = fillAllRows ys (mapRow (\_ a -> 1) y result)
						  | otherwise = fillAllRows ys result

-- analogicznie jak fillAllRows
fillAllCols :: (Num m) => [Int] -> Matrix m -> Matrix m
fillAllCols [] result = result
fillAllCols (x:xs) result | x == (Data.Vector.length (getCol x result)) = fillAllCols xs (mapCol (\_ a -> 1) x result)
						  | otherwise = fillAllCols xs result

-- zwraca listę pozycji, które mogą być zaznaczone od strony lewej w wierszu,
-- pobiera liczbe pozycji, które na pewno muszą być zaznaczone w wierszu oraz długość wiersza
-- zwraca listę pozycji gdzie potencjalnie mogę być zaznaczone pozycje
-- na podstawie Metody Zamalowanych Kratek na Przecięciach
getSelectedFromLeft :: Int -> Int -> [Int]
getSelectedFromLeft numberOfPositions rowLength | numberOfPositions == 0 = []
												| otherwise = [(numberOfPositions - 1)] ++ getSelectedFromLeft (numberOfPositions - 1) rowLength

-- analogicznie jak getSelectedFromLeft tylko tutaj sprawdzenie możliwych
-- do zaznaczenia pozycji następuje od prawej strony
getSelectedFromRight :: Int -> Int -> [Int]
getSelectedFromRight numberOfPositions rowLength | numberOfPositions == 0 = []
												 | otherwise = [(rowLength - 1)] ++ getSelectedFromRight (numberOfPositions - 1) (rowLength - 1)

getListsDuplicates :: [Int] -> [Int] -> [Int]
getListsDuplicates [] _ = []
getListsDuplicates _ [] = []
getListsDuplicates (x:xs) y | x `elem` y = [x] ++ getListsDuplicates xs y
						  	| otherwise = getListsDuplicates xs y

--matrix (length yx) (length xs) $ \(i,j) -> 0

--findPretendents :: [Int] -> [Int] -> [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
