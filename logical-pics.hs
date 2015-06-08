import System.Environment
import System.IO

import Data.Matrix

import qualified Data.Vector
import qualified Debug.Trace 
import qualified Data.Vector.Generic

main = do
	putStrLn "Please type file name with board data:"  
	
	filename <- getLine  
	
	putStrLn ("Opening: " ++ filename)

	handle <- openFile filename ReadMode
	rowList <- hGetLine handle
	colList <- hGetLine handle 
	let rows = read rowList::[Int]
	let cols = read colList::[Int]
	
	putStrLn ("Rows: " ++ rowList)
	putStrLn ("Columns: " ++ colList)
	
	print (findAllHash rows cols)

-- Funkcja inicująca metodę znajdująca wszystkie zamalowane punkty
findAllHash ys xs =  findAllHash' ys xs (matrix (length ys) (length xs) $ \(_,_) -> 0)

findAllHash' :: [Int] -> [Int] -> Matrix Int -> Matrix Int
findAllHash' ys xs result | isAllBoardFilled result = result
						  | otherwise = (Debug.Trace.trace (prettyMatrix result) 					
									(findAllHash' ys xs
										(fillPossibilities ys 1 xs 1 
											(fill2IfMaxVals ys 1 xs 1
												(fillIntersections ys 1 (length ys) xs 1 (length xs) (fillAllCols 1 xs (fillAllRows 1 ys result)))))))

fillPossibilities :: [Int] -> Int -> [Int] -> Int -> Matrix Int -> Matrix Int
fillPossibilities [] _ [] _ result = result
fillPossibilities [] rowIndex (x:xs) colIndex result | countZeroVals (Data.Vector.Generic.toList (getCol colIndex result)) == x 
															 && countOneVals (Data.Vector.Generic.toList (getCol colIndex result)) == 0
														= fillOnesInZerosInCol (Data.Vector.Generic.toList (getCol colIndex result)) 1 colIndex result
													 | countZeroValsAndNear1s (Data.Vector.Generic.toList (getCol colIndex result)) 0 == x 
													 	= fillOnesInZerosInCol (Data.Vector.Generic.toList (getCol colIndex result)) 1 colIndex result
													 | otherwise = fillPossibilities [] rowIndex xs (colIndex+1) result
fillPossibilities (y:ys) rowIndex xs colIndex result | countZeroVals (Data.Vector.Generic.toList (getRow rowIndex result)) == y 
															 && countOneVals (Data.Vector.Generic.toList (getRow rowIndex result)) == 0
														= fillOnesInZerosInRow (Data.Vector.Generic.toList (getRow rowIndex result)) 1 rowIndex result
													 | countZeroValsAndNear1s (Data.Vector.Generic.toList (getRow rowIndex result)) 0 == y 
												 		= fillOnesInZerosInRow (Data.Vector.Generic.toList (getRow rowIndex result)) 1 rowIndex result
													 | otherwise = fillPossibilities ys (rowIndex+1) xs colIndex result

fillOnesInZerosInCol :: [Int] -> Int -> Int -> Matrix Int -> Matrix Int
fillOnesInZerosInCol [] _ _ result = result
fillOnesInZerosInCol (x:xs) colIndex counter result | x == 0 = fillOnesInZerosInCol xs colIndex (counter+1) (setElem 1 (counter, colIndex) result)
													| otherwise = fillOnesInZerosInCol xs colIndex (counter+1) result

fillOnesInZerosInRow :: [Int] -> Int -> Int -> Matrix Int -> Matrix Int
fillOnesInZerosInRow [] _ _ result = result
fillOnesInZerosInRow (x:xs) rowIndex counter result | x == 0 = fillOnesInZerosInRow xs rowIndex (counter+1) (setElem 1 (rowIndex, counter) result)
													| otherwise = fillOnesInZerosInRow xs rowIndex (counter+1) result

fill2IfMaxVals :: [Int] -> Int -> [Int] -> Int -> Matrix Int -> Matrix Int
fill2IfMaxVals [] _ [] _ result = result
fill2IfMaxVals [] rowIndex (x:xs) colIndex result =
		fill2IfMaxVals
			[]
			rowIndex
			xs
			(colIndex+1)
			(fill2InCol (countOneVals (Data.Vector.Generic.toList (getCol colIndex result)))
						(Data.Vector.Generic.toList (getCol colIndex result)) 
						x
						colIndex 
						1
						result)
fill2IfMaxVals (y:ys) rowIndex xs colIndex result =
		fill2IfMaxVals
			ys
			(rowIndex+1) 
			xs 
			colIndex 
			(fill2InRow (countOneVals (Data.Vector.Generic.toList (getRow rowIndex result)))
						(Data.Vector.Generic.toList (getRow rowIndex result)) 
						y
						rowIndex 
						1
						result)

-- zwraca liczbe '1' w liscie
countOneVals :: [Int] -> Int
countOneVals as = length (filter (==1) as)

countZeroVals :: [Int] -> Int
countZeroVals as = length (filter (==0) as)

-- zlicza liczbę ze w sąsiedztwie włącznie z liczbą najbliższych wystapien 1
countZeroValsAndNear1s :: [Int] -> Int -> Int
countZeroValsAndNear1s [] startVal = startVal
countZeroValsAndNear1s (a:as) startVal | a == 1 || a == 0 = countZeroValsAndNear1s as (startVal+1)
									   | otherwise = countZeroValsAndNear1s as 0

-- wstawia 2 jesli w wierszu jest juz konieczna ilosc jedynek
fill2InCol :: Int -> [Int] -> Int -> Int -> Int -> Matrix Int -> Matrix Int
fill2InCol _ [] _ _ _ result = result
fill2InCol oneCount (a:as) val colIndex rowIndex result
	| (oneCount == val) && (a == 0) = fill2InCol oneCount as val colIndex (rowIndex+1) (setElem 2 (rowIndex, colIndex) result)
	| otherwise = fill2InCol oneCount as val colIndex (rowIndex+1) result

-- wstawia 2 jesli w wierszu jest juz konieczna ilosc jedynek
-- analogicznie jak fill2InRow, pewnie da się jakoś ładnie przekazać funkcje i zrobić z tych funkcji zrobić jedną
fill2InRow :: Int -> [Int] -> Int -> Int -> Int -> Matrix Int -> Matrix Int
fill2InRow _ [] _ _ _ result = result
fill2InRow oneCount (a:as) val rowIndex colIndex result
	| (oneCount == val) && (a == 0) = fill2InRow oneCount as val rowIndex (colIndex+1) (setElem 2 (rowIndex, colIndex) result)
	| otherwise = fill2InRow oneCount as val rowIndex (colIndex+1) result

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
