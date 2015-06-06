import Data.Matrix
import qualified Data.Vector
import qualified Debug.Trace 

main = do
	print (findAllHash 
			[1, 4, 3, 2]
			[2, 3, 2, 3])
			--[[1], [4], [3], [2]]
			--[[2], [3], [2], [3]])

-- Funkcja inicująca metodę znajdująca wszystkie zamalowane punkty
findAllHash ys xs =  findAllHash' ys xs (matrix (length ys) (length xs) $ \(_,_) -> 0)

findAllHash' :: [Int] -> [Int] -> Matrix Int -> Matrix Int
findAllHash' ys xs result = 
	--(findAllHash' ys xs (fillAllCols xs (fillAllRows ys result)))
	(Debug.Trace.trace (prettyMatrix result) 
		(findAllHash' ys xs 
			(fillIntersections ys 1 (length ys) xs 1 (length xs) (fillAllCols 1 xs (fillAllRows 1 ys result)))))
--findAllHash' ys xs result = findAllHash' ys xs (fillAllRows ys (fillAllCols xs result))

-- funkcja sprawdzająca czy cała plansza jest wypełniona
isAllBoardFilled :: [Int] -> [Int] -> Matrix Int -> Bool
isAllBoardFilled [] _ _ = True
isAllBoardFilled _ [] _ = True
isAllBoardFilled (y:ys) xs result | y == (Data.Vector.sum (getRow y result)) = False
									  | otherwise = isAllBoardFilled ys xs result

-- funkcja wypelniajaca linie w calosci w przypadku gdy liczba pozycji do zamalowania rowna 
-- jest dlugosci wiersza
-- mapRow (Map a function over a row)
-- uwaga! indeksowanie wierszy macierzy rozpoczyna się od 1
fillAllRows :: Int -> [Int] -> Matrix Int -> Matrix Int
fillAllRows _ [] result = result
fillAllRows rowIndex (y:ys) result | y == (Data.Vector.length (getRow rowIndex result)) = fillAllRows (rowIndex+1) ys (mapRow (\_ a -> 1) rowIndex result)
						  		   | otherwise = fillAllRows (rowIndex+1) ys result

-- analogicznie jak fillAllRows
-- indeksowanie kolumn zaczyna się od 1
fillAllCols :: Int -> [Int] -> Matrix Int -> Matrix Int
fillAllCols _ [] result = result
fillAllCols colIndex (x:xs) result | x == (Data.Vector.length (getCol colIndex result)) = fillAllCols (colIndex+1) xs (mapCol (\_ a -> 1) colIndex result)
						  		   | otherwise = fillAllCols (colIndex+1) xs result

-- zamalowuje przecięcia, zgodnie z metodą Zamalowanych Kratek na Przecięciach
fillIntersections :: [Int] -> Int -> Int -> [Int] -> Int -> Int -> Matrix Int -> Matrix Int
fillIntersections [] _ _ [] _ _ result = result
fillIntersections [] rowIndex rowLength (x:xs) colIndex colLength result = -- potem po kolumnach
	fillIntersections
		[]
		rowIndex
		rowLength
		xs
		(colIndex+1)
		colLength
		(fillElementsInCol
			colIndex
			(getListsDuplicates (getSelectedFromLeft x colLength) (getSelectedFromRight x colLength))
			result)
fillIntersections (y:ys) rowIndex rowLength xs colIndex colLength result = -- najpierw po wierszach
	fillIntersections
		ys
		(rowIndex+1)
		rowLength
		xs
		colIndex
		colLength	
		(fillElementsInRow 
			rowIndex
			(getListsDuplicates (getSelectedFromLeft y rowLength) (getSelectedFromRight y rowLength))
			result)

-- wypelnia elementy w wierszu o podanym indeksie
fillElementsInRow :: Int -> [Int] -> Matrix Int -> Matrix Int
fillElementsInRow _ [] result = result
fillElementsInRow rowIndex (y:ys) result = fillElementsInRow rowIndex ys (setElem 1 (rowIndex, y) result)

-- wypelnia elementy w kolumnie o podanym indeksie
fillElementsInCol :: Int -> [Int] -> Matrix Int -> Matrix Int
fillElementsInCol _ [] result = result
fillElementsInCol colIndex (x:xs) result = fillElementsInCol colIndex xs (setElem 1 (x, colIndex) result)

-- zwraca listę pozycji, które mogą być zaznaczone od strony lewej w wierszu,
-- pobiera liczbe pozycji, które na pewno muszą być zaznaczone w wierszu oraz długość wiersza
-- zwraca listę pozycji gdzie potencjalnie mogę być zaznaczone pozycje
-- na podstawie Metody Zamalowanych Kratek na Przecięciach
-- uwaga! zakładam indeksowanie od 1
getSelectedFromLeft :: Int -> Int -> [Int]
getSelectedFromLeft numberOfPositions rowLength | numberOfPositions == 0 = []
												| otherwise = [numberOfPositions] ++ getSelectedFromLeft (numberOfPositions - 1) rowLength

-- analogicznie jak getSelectedFromLeft tylko tutaj sprawdzenie możliwych
-- do zaznaczenia pozycji następuje od prawej strony
-- uwaga! zakładam indeksowanie od 1
getSelectedFromRight :: Int -> Int -> [Int]
getSelectedFromRight numberOfPositions rowLength | numberOfPositions == 0 = []
												 | otherwise = [rowLength] ++ getSelectedFromRight (numberOfPositions - 1) (rowLength - 1)

getListsDuplicates :: [Int] -> [Int] -> [Int]
getListsDuplicates [] _ = []
getListsDuplicates _ [] = []
getListsDuplicates (x:xs) y | x `elem` y = [x] ++ getListsDuplicates xs y
						  	| otherwise = getListsDuplicates xs y
