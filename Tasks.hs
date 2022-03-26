{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

{-
	PP Project 2021

	This is where you will write the implementation for the given tasks.
	You can add other modules aswell.
-}

module Tasks where

import Dataset
import Text.Printf
import Data.Char
import Data.List
import Data.Function


type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]
--Table =  [[String]]

{-
	TASK SET 1
-}

-- Task 1
compute_exam_grades :: Table -> Table
compute_exam_grades [] = []
compute_exam_grades tabel = (first (head tabel)) : (map op (tail tabel)) where
                                op :: Row -> Row
                                op [] = []
                                op linie = (head linie) : [(printf "%.2f"(((toFloat (suma (init(tail linie)))) / 4) + (readFloat (last linie))))]



readEl :: String -> Integer
readEl x = read x :: Integer

readFloat :: String -> Float
readFloat x    | x == "" = 0 :: Float
               | otherwise = read x :: Float

makeInteger :: Int -> Integer
makeInteger x = fromIntegral x :: Integer

toFloat :: Integer -> Float
toFloat x = fromInteger x :: Float

--obtinerea antetului neceasar cu Nume si Punctaj examen
first :: Row -> Row
first [] = []
first linie = (head linie) : ["Punctaj Exam"]

--calculul sumei intregi a numerelor reprezentate ca string. De ex ["1", "2"]..
suma :: [String] -> Integer
suma [] = 0
suma (x:xs)    | x == "" = 0 + (suma xs)
               |otherwise = (readEl x) + (suma xs)



-- Task 2
-- Number of students who have passed the exam:
get_passed_students_num :: Table -> Int
get_passed_students_num tabel = foldr op 0 (tail (compute_exam_grades tabel)) where
                                      op linie acc    | (readFloat(last linie) >= 2.5) = 1 + acc
                                                      | otherwise = 0 + acc


-- Percentage of students who have passed the exam:
get_passed_students_percentage :: Table -> Float
get_passed_students_percentage tabel = (toFloat (makeInteger (get_passed_students_num tabel))) / (toFloat(makeInteger((length tabel) - 1)))

-- Average exam grade
get_exam_avg :: Table -> Float
get_exam_avg tabel = foldr op 0 (tail (compute_exam_grades tabel)) where
                           op linie acc = (readFloat(last linie)) / (toFloat(makeInteger((length tabel) - 1))) + acc

-- Number of students who gained at least 1.5p from homework:
get_passed_hw_num :: Table -> Int
get_passed_hw_num tabel = foldr op 0 (tail tabel) where
                                  op linie acc    | ((sumaFloat (tail (tail (init (init (init (init linie))))))) >= 1.5) = 1 + acc
                                                  | otherwise = 0 + acc


sumaFloat :: [String] -> Float
sumaFloat [] = 0
sumaFloat (x:xs)    | x == "" = 0 + (sumaFloat xs)
                    |otherwise = (readFloat x) + (sumaFloat xs)



-- Task 3
get_avg_responses_per_qs :: Table -> Table
get_avg_responses_per_qs tabel = tail (init (head tabel)) : [(impart tabel)]


--Obtinerea unui tabel doar cu punctele la Q1, Q2, Q3, Q4, Q5, Q6
aux :: Table -> Table
aux tabel = map op (tail tabel) where
                op :: Row -> Row
                op [] = []
                op linie = tail (init linie)


transposem :: Table -> Table
transposem ([]:_) = []
transposem m = (map head m) : (transposem (map tail m))

--calculul sumei punctelor la fiecare Q
calculate :: Table -> [Float]
calculate tabel = map sumaFloat (transposem (aux tabel))

--un fel de blackbox, care, primin vectorul cu sumele de punctaje, afla media si o expune ca string
impart :: Table -> [String]
impart tabel = map op (calculate tabel) where
                   op el = printf "%.2f" (el / toFloat (makeInteger ((length tabel) - 1)))



-- Task 4 
get_exam_summary :: Table -> Table
get_exam_summary tabel = ["Q", "0", "1", "2"] : (transposem (total tabel))  

--suma de punctaje de 0
vectorof0 :: Table -> [Integer]
vectorof0 tabel = map op (transposem (aux tabel)) where
                  op [] = 0
                  op (x:xs)    | (x == "0") = 1 + (op xs) 
                               | (x == "")  = 1 + (op xs)
                               | otherwise  = 0 +  op xs 
--suma de punctaje de 1
vectorof1 :: Table -> [Integer]
vectorof1 tabel = map op (transposem (aux tabel)) where
                  op [] = 0
                  op (x:xs)    | (x == "1") = 1 + (op xs) 
                               | otherwise  = 0 +  op xs 
--suma de punctaje de 2
vectorof2 :: Table -> [Integer]
vectorof2 tabel = map op (transposem (aux tabel)) where
                  op [] = 0
                  op (x:xs)    | (x == "2") = 1 + (op xs) 
                               | otherwise  = 0 +  op xs 

make :: [Integer] -> Row
make [] = []
make (x:xs) = (show x) : make xs

--un fel de blackbox pentru ca functia get_exam_summary sa fie mai frumoasa :)
--adun intr-un tabel vectorul de Q-uri si punctajele aferente, insa doar sub forma de linii dintr-o matrice
--deci va fi nevoie de transpus cu functia transpose mai tarziu pentru a obtine tabela necesara
total :: Table -> Table
total tabel = (init (tail (head tabel))) : (make(vectorof0 tabel) : ((make (vectorof1 tabel) : [make(vectorof2 tabel)])))



-- Task 5
get_ranking :: Table -> Table
get_ranking tabel = (head (compute_exam_grades tabel)) : (blackBox tabel)


blackBox :: Table -> Table
blackBox [] = []
blackBox tabel = toTable (sortBy myCompare (toTouple (tail (compute_exam_grades tabel))))


--functia de comparare aplicata la sortBy
myCompare ::  (String,String) -> (String,String) -> Ordering
myCompare (nume1,nota1) (nume2,nota2)
    | (nota1 < nota2) = LT
    | ((nota1 == nota2) && (nume1 < nume2)) = LT
    | ((nota1 == nota2) && (nume1 > nume2)) = GT
    | otherwise = GT


toTouple :: Table -> [(String,String)]
toTouple [] = []
toTouple ((x : profyle) : tabel) = (x,(last profyle)) : (toTouple tabel) 

toTable :: [(String, String)] -> Table
toTable [] = []
toTable ((x,y) : tabel) = (x : [y]) : (toTable tabel)



-- Task 6
get_exam_diff_table :: Table -> Table
get_exam_diff_table tabel = (firstRow (head tabel)) : (blackBox2 tabel)

{-============ Compare peste (Nume, Interviu, Ex Scris, Diff Block) Block =============-}

blackBox2 :: Table -> Table
blackBox2 [] = []
blackBox2 tabel = toTableTable (sortBy myCompare2 (toToupleTouple (bigTable tabel)))

{-============ Compare Block =============-}

myCompare2 ::  ((String,String),(String,String))  -> ((String,String),(String,String)) -> Ordering
myCompare2 ((nume1,interviu1),(scris1,dif1)) ((nume2,interviu2),(scris2,dif2)) 
    | (dif1 < dif2) = LT
    | ((dif1 == dif2) && (nume1 < nume2)) = LT
    | otherwise = GT



toToupleTouple :: Table -> [((String,String),(String,String))]
toToupleTouple [] = []
toToupleTouple (profyle : tabel) = ((head profyle, head (tail profyle)), (head(tail(tail profyle)), last profyle)) : (toToupleTouple tabel) 

toTableTable :: [((String,String),(String,String))] -> Table
toTableTable [] = []
toTableTable (((n1,or1),(sc1,dif1)) : tabel) = (n1:(or1:(sc1:([dif1])))) : (toTableTable tabel)


{-============ Nume, Interviu, Ex Scris, Diff Block =============-}

--antetul necesar al matricii
firstRow :: Row -> Row
firstRow [] = []
firstRow linie = (head linie) : ["Punctaj interviu","Punctaj scris","Diferenta"] 

--tabel cu punctaje orale
punctaj_oral :: Table -> Table
punctaj_oral [] = []
punctaj_oral tabel = map aduna (aux tabel) where
                         aduna linie = [printf "%.2f" ((sumaFloat linie) / 4)]

--tabel cu punctaje la ex scris
punctaj_scris :: Table -> Table
punctaj_scris [] = []
punctaj_scris tabel = map op (tail tabel) where
                          op linie = [(printf "%.2f" (readFloat (last linie)))]

--diferenta intre acele 2 punctaje
diffTable :: Table -> Table -> Table
diffTable [] [] = []
diffTable (x:xs) (y:ys) = [printf "%.2f" ((abs ((readFloat (head x)) - (readFloat (head y)))))] : (diffTable xs ys)

--pentru compunerea punctajelor scrise si diferenta
someGradesTogether :: Table -> Table -> Table
someGradesTogether [] [] = []
someGradesTogether ((x:xs):xss) ((y:ys):yss) = (x : [y]) : (someGradesTogether xss yss)

--pentru compunerea pucntajelor orale si tabelului de punctaje scrise si diferenta
allGradesTogether :: Table -> Table -> Table
allGradesTogether [] [] = []
allGradesTogether  ((x:xs):xss) (y:ys) = (x : y) : (allGradesTogether xss ys)

--tabel cu nume
namesTable :: Table -> Table
namesTable [] = []
namesTable tabel = map op (tail tabel) where
                       op linie = [head linie]
--tabelul cu "Punctaj interviu","Punctaj scris","Diferenta"
bigTable :: Table -> Table
bigTable [] = []
bigTable tabel = allGradesTogether (namesTable tabel) (allGradesTogether (punctaj_oral tabel) (someGradesTogether (punctaj_scris tabel) (diffTable (punctaj_oral tabel) (punctaj_scris tabel))))



{-================      SET 2     =================-}

-- Prerechizit 1

read_csv :: CSV -> Table
read_csv str =  (map (\x -> splitBy ',' x) (splitBy '\n' str))


splitBy :: Char -> String -> [String]
splitBy c phrase = foldr op [""] phrase where
    op x (y : ys)
        | (x == c) = [] : (y:ys)
        | otherwise = (x : y) : ys


-- Prerechizit 2

write_csv :: Table -> CSV
write_csv tabel = tail $ concatStr1 (map ("\n" ++) (map concatStr2 tabel))

concatStr :: [String] -> String
concatStr [] = []
concatStr (x:list) = x ++ "," ++ (concatStr list)

concatStr1 :: [String] -> String
concatStr1 [] = []
concatStr1 (x:list) = x ++ (concatStr1 list)

--no final coma
concatStr2 :: [String] -> String
concatStr2 [] = []
concatStr2 str = (init (concatStr str))


-- Spoiler : Vor fi putine comentarii caci am stat 3 zile la task7 si nu mai tin minte pederost ce fac functiile :D 

--Task 1
as_list :: String -> Table -> [String]
as_list column (x:xs) = rowMatrix xs (find_column column (x:xs)) where
      find_column :: String -> Table -> Int
      find_column name (x:xs) = auxiliar 0 x where
          -- gasirea indexului coloanei
          auxiliar count (x:xs) 
           | name == x = count
           | otherwise = auxiliar (count + 1) xs

--functie ce returneaza o coloana dintr-un tabel dupa index
rowMatrix :: Table -> Int -> [String]
rowMatrix table index = map (!! index) table


numerical :: String -> Bool
numerical "" = True
numerical (x:xs) = if x <= '9' && x >= '0' then True else False

-- Tassk 2
-- sortarea cu propria functie de comparatie .
tsort :: String -> Table -> Table
tsort column tabel = (head tabel) : (sortBy cmp (tail tabel)) where
    findCol :: String -> Table -> Int
    findCol name (x:xs) = auxiliar 0 x where
        auxiliar count (x:xs)
          | name == x = count
          | otherwise = auxiliar (count + 1) xs

    cmp :: Row -> Row -> Ordering
    --daca sunt egale, compara dupa nume. Otherwise, dupa note.
    cmp row1 row2 | numerical (head (drop (findCol column tabel) row1)) = numericalCMP (head (drop (findCol column tabel) row1)) (head (drop (findCol column tabel) row2))
                  | cmp2 (head (drop (findCol column tabel) row1)) (head (drop (findCol column tabel) row2)) == EQ = cmp2 (head row1) (head row2)
                  | otherwise = cmp2 (head (drop (findCol column tabel) row1)) (head (drop (findCol column tabel) row2))
      where 
        cmp2 :: String -> String -> Ordering
        cmp2 e1 e2
          | e1 < e2 = LT
          | e1 > e2 = GT
          | otherwise = EQ

        numericalCMP x y
          | (readFloat x) < (readFloat y) = LT
          | (readFloat x) > (readFloat y) = GT
          | otherwise = cmp2 (head row1) (head row2)




-- Task 3
--functia f peste fiecare element al tabelului
vmap :: (Value -> Value) -> Table -> Table
vmap f tabel = map op tabel where
                   op [] = []
                   op (x:xs) = (f x) : (op xs)



-- Task 4
--functia get_hw_grade_total peste liniile tabelului
rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap f list tabel = list : (map f (tail tabel))

get_hw_grade_total :: Row -> Row
get_hw_grade_total linie = (head linie) : [(printf "%.2f" (sumaFloat (tail (tail linie))))]



-- Task 5
vunion :: Table -> Table -> Table
vunion [] [] = []
vunion t1 t2    | head t1 == head t2 = t1 ++ (tail t2)     -- daca au aceleasi coloane, la sfarsitul lui t1 se baga liniile din t2
                | otherwise = t1 



-- Task 6
hunion :: Table -> Table -> Table
hunion t1 t2    | length t1 == length t2 = zipWith (++) t1 t2
                | length t1 > length t2 = zipWith (++) t1 (t2 ++ (adjustmentForT2 ((length t1) - (length t2)) (length (last t2))))
                | otherwise = zipWith (++) (t1 ++ (adjustmentForT2 ((length t2) - (length t1)) (length (last t1)))) t2 

--completarea tabelului 2 cu linii de elemente ""
adjustmentForT2 :: Int -> Int -> Table
adjustmentForT2 0 _ = []
adjustmentForT2 nrLines nrElems = [(take nrElems (repeat ""))] ++ (adjustmentForT2 (nrLines - 1) nrElems)

 
--completarea tabelului 1 cu linii de elemente ""
adjustmentForT1 :: Int -> Int -> Table
adjustmentForT1 0 _ = []
adjustmentForT1 nrLines nrElems = [(take nrElems (repeat ""))] ++ (adjustmentForT1 (nrLines - 1) nrElems)



-- Task 7
--Here we go, hardest problem in the world
-- folosesc functia concatColumn pentru a obtine [Table], introducand numele coloanei in fiecare element al acestei coloane.
tjoin :: String -> Table -> Table -> Table
--pentru fiecare tabela din [Table], obtinut prin concatColumn asupra tabel1, caut intrarea in (concatColumn tabel2) cu functiile
--findtheRow si findtheValue, si aplic functia conc peste ele
tjoin column tabel1 tabel2 = auxiliarF (map (\t -> conc t (findtheRow column (findtheValue column t) (concatColumn tabel2))) (concatColumn tabel1)) where
   
    auxiliarF tabel = (headers tabel) : (values tabel)

    headers tabel = map head (head tabel)

    values tabel = map (map (head.tail)) tabel


findtheRow :: String -> String -> [Table] -> Table
findtheRow key value tabel = search tabel where
    search [] = map (\[key, value] -> [key, ""]) (head tabel)
    search (x:xs) | (findtheValue key x) == value = x
                  | otherwise = search xs

findtheValue :: String -> Table -> String
findtheValue key ([k, v]:xs) | key == k = v
                           | otherwise = findtheValue key xs


--concatenarea tabelelor de forma ([key, value] : xs)
conc :: Table -> Table -> Table
conc x y = foldl op x y where
    op [] entry = [entry]
    op ([key1, value1] : xs) [key2, value2]    | (key1 == key2) = [key1, (choose value1 value2)] : xs
                                               | otherwise = [key1, value1] : (op xs [key2, value2])

    choose value1 "" = value1
    choose _ value2  = value2



-- Blockul care obtine tabela cu numele coloanei introdus pe fiecare rand in forma [key, value]
-- am impartit pe mai multi pasi pentru o lizibilitate mai mare. Practica din momentele de plina derutare.
each :: Row -> Table
each row = map (:[]) row

mapEach :: Table -> [Table]
mapEach table = map each table

concatColumn :: Table -> [Table]
concatColumn table = map (\t -> zipWith (++) (head (mapEach table)) t) (tail (mapEach table))




-- Task 8  
--function f on each entry in t1 with each entry in t2
--folosirea a 2 mapuri pentru a parcurge ca un fel de for in for
cartesian :: (Row -> Row -> Row) -> [String] -> Table -> Table -> Table
cartesian func header t1 t2 = header : (foldr (++) [] $ map (\row1 -> map (\row2 -> (func row1 row2)) (tail t2)) (tail t1))



-- Task 9
--coincide putin cu ideea taskului 1, doar ca trebuie de unit acele coloane. Am facut asta cu hunion.
projection :: [String] -> Table -> Table
projection [] (x:xs) = [[]]
projection (c:columns) (x:xs) = hunion (map (:[]) (rowMatrix (x:xs) (find_column c (x:xs)))) (projection columns (x:xs)) where
      find_column :: String -> Table -> Int
      find_column name (x:xs) = auxiliar 0 x where
          auxiliar count (x:xs)
           | name == x = count
           | otherwise = auxiliar (count + 1) xs





{-================      SET 3     =================-}

data Query =
    FromCSV CSV
    | ToCSV Query
    | AsList String Query
    | Sort String Query
    | ValueMap (Value -> Value) Query
    | RowMap (Row -> Row) [String] Query
    | VUnion Query Query
    | HUnion Query Query
    | TableJoin String Query Query
    | Cartesian (Row -> Row -> Row) [String] Query Query
    | Projection [String] Query
    | forall a. FEval a => Filter (FilterCondition a) Query
    | Graph EdgeOp Query


-- where EdgeOp is defined:
type EdgeOp = Row -> Row -> Maybe Value

--  3.1
data QResult = CSV CSV | Table Table | List [String]

instance Show QResult where
  show (List []) = []
  show (CSV tabel_csv) = show tabel_csv
  show (Table tabel) = write_csv tabel
  show (List list) = show list

transformToTable :: QResult -> Table
transformToTable (Table table) = table

transformToStr :: Maybe Value -> Value
transformToStr (Just x) = x

--  3.2
class Eval a where
    eval :: a -> QResult


instance Eval Query where
  eval (FromCSV str) = Table (read_csv str)
  eval (ToCSV query) = CSV (write_csv (transformToTable (eval query)))
  eval (AsList str query) = List (as_list str (transformToTable (eval query)))
  eval (Sort str query) = Table (tsort str (transformToTable (eval query)))
  eval (ValueMap func query) = Table (vmap func (transformToTable (eval query)))
  eval (RowMap func row query) = Table (rmap func row (transformToTable (eval query)))
  eval (VUnion query1 query2) = Table (vunion (transformToTable (eval query1)) (transformToTable (eval query2)))
  eval (HUnion query1 query2) = Table (hunion (transformToTable (eval query1)) (transformToTable (eval query2)))
  eval (TableJoin str query1 query2) = Table (tjoin str (transformToTable (eval query1)) (transformToTable (eval query2)))
  eval (Cartesian func row query1 query2) = Table (cartesian func row (transformToTable (eval query1)) (transformToTable (eval query2)))
  eval (Projection row query) = Table (projection row (transformToTable (eval query)))
  eval (Filter filterCond query) = Table (filt (transformToTable (eval query))) where
    filt :: Table -> Table
    filt (numele_coloanelor:tabel) = numele_coloanelor : (filter (feval numele_coloanelor filterCond) tabel)

  eval (Graph edgeop query) = Table (double_for_func (tail (transformToTable (eval query)))) where    
                                     double_for_func [] = []
                                     double_for_func table = foldl op [["From", "To", "Value"]] table
                                         where
                                          op acc [] = acc
                                          op acc row1 = foldl (\acc2 row2 -> if ((edgeop row1 row2) /= Nothing) 
                                            then (if ((head row1) /= (head row2)) then (acc2 ++ [[(min (head row1) (head row2)),(max (head row1) (head row2)),(transformToStr (edgeop row1 row2))]]) else (acc))
                                            else (acc2)) acc table



data FilterCondition a =
    Eq String a |
    Lt String a |
    Gt String a |
    In String [a] |
    FNot (FilterCondition a) |
    FieldEq String String

type FilterOp = Row -> Bool


find_index :: [String] -> String -> Int
find_index list elem = auxx list 0 where
          auxx (x:xs) count
           | elem == x = count
           | otherwise = auxx xs (count + 1)

isInList elem [] = False
isInList elem (x:xs)
    | elem == x = True
    | otherwise = isInList elem xs

class FEval a where
    feval :: [String] -> (FilterCondition a) -> FilterOp


instance FEval Float where
  feval numele_coloanelor (Eq nume_coloana ref) = \row -> ref == (readFloat (row !! (find_index numele_coloanelor nume_coloana)))
  feval numele_coloanelor (Lt nume_coloana ref) = \row -> ref > (readFloat (row !! (find_index numele_coloanelor nume_coloana)))
  feval numele_coloanelor (Gt nume_coloana ref) = \row -> ref < (readFloat (row !! (find_index numele_coloanelor nume_coloana)))
  feval numele_coloanelor (In nume_coloana list) = \row -> isInList (readFloat (row !! (find_index numele_coloanelor nume_coloana))) list
  feval numele_coloanelor (FNot filterCondition) = \row -> not (feval numele_coloanelor  (filterCondition) row)
  feval numele_coloanelor (FieldEq c1 c2) = \row -> (readFloat (row !! (find_index numele_coloanelor c1))) == (readFloat (row !! (find_index numele_coloanelor c2)))

instance FEval String where
  feval numele_coloanelor (Eq nume_coloana ref) = \row -> ref == (row !! (find_index numele_coloanelor nume_coloana))
  feval numele_coloanelor (Lt nume_coloana ref) = \row -> ref > (row !! (find_index numele_coloanelor nume_coloana))
  feval numele_coloanelor (Gt nume_coloana ref) = \row -> ref < (row !! (find_index numele_coloanelor nume_coloana))
  feval numele_coloanelor (In nume_coloana list) = \row -> isInList (row !! (find_index numele_coloanelor nume_coloana)) list
  feval numele_coloanelor (FNot filterCondition) = \row -> not (feval numele_coloanelor  (filterCondition) row)
  feval numele_coloanelor (FieldEq c1 c2) = \row -> (row !! (find_index numele_coloanelor c1)) == (row !! (find_index numele_coloanelor c2))


similarities_query :: Query
similarities_query = Sort "Value" (Graph edgeop (FromCSV lecture_grades_csv)) where
    edgeop (rh1:rt1) (rh2:rt2) = adjust (length (filter (\x -> x == True) (zipWith (==) rt1 rt2))) where
        adjust len = if len >= 5 && rh1 /= "" && rh2 /= "" then Just (show len) else Nothing




{-================      SET 4     =================-}

--Nothing to see :(

correct_table :: String -> CSV -> CSV -> CSV
correct_table colname t ref = write_csv [(findd (as_list colname (read_csv t)) (as_list colname (read_csv ref)))]

findd :: Row -> Row -> Row
findd [] _ = []
findd (x:row1) (y:row2) | x == y = findd row1 row2
                        | otherwise = x : (findd row1 row2) 


grades :: CSV -> CSV -> CSV -> CSV -> CSV
grades tab1 tab2 tab3 tab4 = undefined