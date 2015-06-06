import Data.Matrix
import qualified Data.Vector
import qualified Debug.Trace 
import qualified Data.Vector.Generic

main = do
	print (findAllHash 
			[2, 4, 3, 1]
			[2, 3, 2, 3])
			--[[1], [4], [3], [2]]
			--[[2], [3], [2], [3]])

-- Funkcja inicująca metodę znajdująca wszystkie zamalowane punkty
findAllHash ys xs =  findAllHash' ys xs (matrix (length ys) (length xs) $ \(_,_) -> 0)

findAllHash' :: [Int] -> [Int] -> Matrix Int -> Matrix Int
findAllHash' ys xs result | isAllBoardFilled result = result
						  | otherwise = (Debug.Trace.trace (prettyMatrix result) 
								(findAllHash' ys xs 
									(fill2IfMaxVals ys 1 xs
										(fillIntersections ys 1 (length ys) xs 1 (length xs) (fillAllCols 1 xs (fillAllRows 1 ys result))))))

fill2IfMaxVals :: [Int] -> Int -> [Int] -> Matrix Int -> Matrix Int
fill2IfMaxVals [] _ _ result = result
fill2IfMaxVals (y:ys) rowIndex xs result =
		fill2IfMaxVals
			ys 	
			(rowIndex+1) 
			xs 
			(fill2InRow (sum (Data.Vector.Generic.toList (getRow rowIndex result)))
						(Data.Vector.Generic.toList (getRow rowIndex result)) 
						y
						rowIndex 
						1
						result)

-- wstawia 2 jesli w wierszu jest juz konieczna ilosc jedynek
fill2InRow :: Int -> [Int] -> Int -> Int -> Int -> Matrix Int -> Matrix Int
fill2InRow _ [] _ _ _ result = result
fill2InRow listSum (a:as) val rowIndex colIndex result
	| listSum > val = result
	| (listSum == val) && (a == 0) = fill2InRow listSum as val rowIndex (colIndex+1) (setElem 2 (rowIndex, colIndex) result)
	| otherwise = fill2InRow listSum as val rowIndex (colIndex+1) result

-- funkcja sprawdzająca czy cała plansza jest wypełniona
-- jeśli na każdej pozycji jest wartość większa niż 0 to KONIEC
isAllBoardFilled :: Matrix Int -> Bool
isAllBoardFilled result = all (>0) (toList result)

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
