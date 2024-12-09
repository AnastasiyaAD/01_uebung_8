-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
--------------------------------------------------------------------------Aufgabenblatt 6 .hs--------------------------------------------------------------------------
module Mengen where
type Fehlermeldung = String
type MengeAlsZeichenreihe = String
newtype MT1 e = MT1 [e] 
data MT2 e = Nichts
             | VerlaengereUm e (MT2 e)
newtype MT3 e = MT3 (e -> Bool)


--Klasse Defaultable
--Definiert eine Typklasse für Typen, die einen Standardwert (eine Liste von Werten) liefern können.

class Defaultable a where
  defaultValue :: [a] --  liefert eine Liste von Standardwerten des Typs 'a'.

instance Defaultable Int where
  defaultValue = [(- 100)..100]

instance Defaultable Char where
  defaultValue = ['a'..'z'] ++ ['A'..'Z']

class Menge m where
  leereMenge :: m
  allMenge :: m
  istMenge :: m -> Bool
  vereinige :: m -> m -> m
  schneide :: m -> m -> m
  zieheab :: m -> m -> m
  komplementiere :: m -> m
  komplementiere = zieheab allMenge
  sindGleich :: m -> m -> Bool
  sindGleich m1 m2 = istTeilmenge m1 m2 && istTeilmenge m2 m1 --Zwei Mengen sind gleich, wenn sie Teilmengen voneinander sind
  sindUngleich :: m -> m -> Bool
  sindUngleich m1 = not . sindGleich m1
  istTeilmenge :: m -> m -> Bool
  istObermenge :: m -> m -> Bool
  istObermenge m1 m2 = istTeilmenge m2 m1
  istEchteTeilmenge :: m -> m -> Bool
  istEchteTeilmenge m1 m2 = istTeilmenge m1 m2 && not (sindGleich m1 m2) --Eine Menge ist echte Teilmenge einer Anderen, wenn sie Teilmenge aber nicht gleich ist
  istEchteObermenge :: m -> m -> Bool
  istEchteObermenge m1 m2 = istEchteTeilmenge m2 m1 --Wenn A (echte) Obermenge von B ist, ist dann B (echte) Teilmenge von A
  sindElementeFremd :: m -> m -> Bool
  sindElementeFremd m1 = sindGleich leereMenge . schneide m1 --Zwei Mengen sind elementefremd, wenn ihrer Schnitt die Leeremenge ist
  sindQuerUeberlappend :: m -> m -> Bool
  sindQuerUeberlappend m1 m2 =             --Zwei Mengen sind quer-> ueberlappend, wenn sie...
    not (sindElementeFremd m1 m2)          --  ... mindestens ein Element gemeinsam haben
    && not (istTeilmenge m1 m2)            --  ... jeweils keine Teilmenge voneinander sind
    && not (istTeilmenge m2 m1)
  istKeinGueltigerMengenwert :: Fehlermeldung -> m
  istKeinGueltigerMengenwert = error
  nichtImplementierbar :: Fehlermeldung -> m
  nichtImplementierbar = error
  zeige :: m -> MengeAlsZeichenreihe


instance Menge (MT1 Char) where
  leereMenge = MT1 []
  allMenge = MT1 defaultValue
  istMenge = istMengeMT1
  vereinige = vereinigeMT1
  schneide = schneideMT1
  zieheab = zieheabMT1
  istTeilmenge = istTeilmengeMT1
  zeige = zeigeMT1


instance Menge (MT1 Int) where
  leereMenge = MT1 []
  allMenge = MT1 defaultValue
  istMenge = istMengeMT1
  vereinige = vereinigeMT1
  schneide = schneideMT1
  zieheab = zieheabMT1
  istTeilmenge = istTeilmengeMT1
  zeige = zeigeMT1


--Allgemeine Funktionen für MT1

istMengeMT1 :: Eq e =>MT1 e -> Bool
istMengeMT1 (MT1     []) = True
istMengeMT1 (MT1 (e:es)) = all (/= e) es && (istMengeMT1 . MT1) es

vereinigeMT1 :: Eq e =>MT1 e -> MT1 e -> MT1 e
vereinigeMT1 m1@(MT1 list1) m2@(MT1 list2)
      | istMengeMT1 m1 && istMengeMT1 m2 = MT1 . nub $ list1 ++ list2
      | otherwise                        = fehlermeldung

schneideMT1 :: Eq e =>MT1 e -> MT1 e -> MT1 e
schneideMT1 m1@(MT1 list1) m2@(MT1 list2)
      | istMengeMT1 m1 && istMengeMT1 m2 = MT1 $ [e | e <-  list1, e `elem` list2]
      | otherwise                        = fehlermeldung

zieheabMT1 :: Eq e =>MT1 e -> MT1 e -> MT1 e
zieheabMT1 m1@(MT1 list1) m2@(MT1 list2)
      | istMengeMT1 m1 && istMengeMT1 m2 = MT1 $ [e | e <-  list1, e `notElem` list2]
      | otherwise                        = fehlermeldung


istTeilmengeMT1 :: Eq e =>MT1 e -> MT1 e -> Bool
istTeilmengeMT1 m1@(MT1 list1) m2@(MT1 list2)
      | istMengeMT1 m1 && istMengeMT1 m2 = all (`elem` list2) list1
      | otherwise                        = fehlermeldung

zeigeMT1 :: Show e =>MT1 e -> MengeAlsZeichenreihe
zeigeMT1 (MT1 elems) = "{" ++ formatElems elems ++ "}"


--Hilffunktionen für MT1.

--Fehlermeldung für wenn ein oder mehrere Argumente nicht Menge sind.

fehlermeldung :: a
fehlermeldung = error "Argument muss Menge sein (keine Duplikate)"

--Entferne Duplikate einer Liste.

nub :: Eq a =>[a] -> [a]
nub     [] = []
nub (e:es) = e : (nub $ filter (/= e) es)

--Formatiere Elemente, um sie auszudrucken.

formatElems :: Show a =>[a] -> String
formatElems []     = ""
formatElems [e]    = show e
formatElems (e:es) = show e ++ ", " ++ formatElems es

instance Menge (MT3 Char) where
  leereMenge = MT3 (\_ -> False)
  allMenge = MT3 (\_ -> True )
  istMenge = \_ -> True
  vereinige = vereinigeMT3
  schneide = schneideMT3
  zieheab = zieheabMT3
  istTeilmenge = istTeilmengeMT3
  zeige = zeigeMT3


instance Menge (MT3 Int) where
  leereMenge = MT3 (\_ -> False)
  allMenge = MT3 (\_ -> True )
  istMenge = \_ -> True
  vereinige = vereinigeMT3
  schneide = schneideMT3
  zieheab = zieheabMT3
  istTeilmenge = istTeilmengeMT3
  zeige = zeigeMT3


--Allgemeine Funktionen für MT3.

vereinigeMT3 :: Eq e =>MT3 e -> MT3 e -> MT3 e
vereinigeMT3 (MT3 f1) (MT3 f2) = MT3 $ \elem -> f1 elem || f2 elem

schneideMT3 :: Eq e =>MT3 e -> MT3 e -> MT3 e
schneideMT3 (MT3 f1) (MT3 f2) = MT3 $ \elem -> f1 elem && f2 elem

zieheabMT3 :: Eq e =>MT3 e -> MT3 e -> MT3 e
zieheabMT3(MT3 f1) (MT3 f2) = MT3 $ \elem -> f1 elem && (not . f2) elem

istTeilmengeMT3 :: (Eq e, Defaultable e) =>MT3 e -> MT3 e -> Bool
istTeilmengeMT3 m1 (MT3 f) =
    let elems1 = toListMT3 m1
    in all f elems1

zeigeMT3 m = "{" ++ (formatElems . toListMT3) m ++ "}"


--Diese Funktion wandelt einen Wert des Typs MT3 in eine Liste vom Typ e um.
--Sie benoetigt die Typklassebeschraenkung (Defaultable e), um sicherzustellen, dass der Typ e eine defaultValue Funktion besitzt.
--Haette man nicht diese Beschaenkung, muesste man Bound nutzen. Es ist aber sehr gross bei Int und Char.

toListMT3 :: (Defaultable e) =>MT3 e -> [e]
toListMT3 (MT3 f) = filter f defaultValue


--Ueberpruefe ob ein Char Element in einer Menge ueber Chars ist.
--Man kann hier bei der Ueberpruefung, ob die Eingaben gueltig sind, nicht istKeinGueltigerMengenwert nutzen, weil der Rueckgabetyp dieser Funktion ein Bool ist, und von istKeinGueltigerMengenwert eine Menge.

istElement :: Menge m =>Char -> m -> Bool
istElement c m
    | not $ isDefaultChar c = error "Ungueltiger Charakter"
    | otherwise             = elem c $ zeige m

--Ueberpruefe ob ein Char kein Element in einer Menge ueber Chars ist.

istKeinElement :: Menge m =>Char -> m -> Bool
istKeinElement c = not . istElement c

isDefaultChar :: Char -> Bool
isDefaultChar = (`elem` defaultValue)

-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
--------------------------------------------------------------------------Aufgabenblatt 7 .hs--------------------------------------------------------------------------

data Landeshauptstadt = B | E | G | I | K | L | P | S | W deriving (Show, Eq, Ord, Bounded, Enum) -- Show, Eq, Ord, Bounded und Enum sind sinnvoll für eine Aufzählung von Landeshauptstädten
-- Man darf nicht `deriving` fuer `type` Deklarationen
--  nutzen, weil es nur ein Typsynonym ist
type Staedtepaar = (Landeshauptstadt,Landeshauptstadt)
-- Show, Eq, Ord sind sinnvoll, da man Paare von Landeshauptstädten vergleichen und ausgeben kann. Bounded und Enum nicht anwendbar, da keine natürliche Ordnung aller Paare existiert.
data Staedtepaar1 = SP1 Landeshauptstadt Landeshauptstadt deriving (Show, Eq, Ord) 
newtype Staedtepaar2 = SP2 (Landeshauptstadt,Landeshauptstadt) deriving (Show, Eq, Ord) -- Ähnlich wie Staedtepaar1
-- Man kann nicht `deriving` fuer Typen nutzen,
--  die charakteristische Funktionen sind
data Staedtepaar3 = SP3 ((Landeshauptstadt,Landeshauptstadt) -> Bool) 
data Staedtepaar4 = SP4 (Landeshauptstadt -> Landeshauptstadt -> Bool) -- Ähnlich wie Staedtepaar3


instance Defaultable Landeshauptstadt where
    defaultValue = [minBound .. maxBound]
-- (a)
instance Menge (MT1 Landeshauptstadt) where
    leereMenge   = MT1 []
    allMenge     = MT1 defaultValue
    istMenge     = istMengeMT1
    vereinige    = vereinigeMT1
    schneide     = schneideMT1
    zieheab      = zieheabMT1
    istTeilmenge = istTeilmengeMT1
    zeige        = zeigeMT1

-- (b)
instance Menge (MT3 Landeshauptstadt) where
  leereMenge   = MT3 (\_ -> False)
  allMenge     = MT3 (\_ -> True )
  istMenge     = \_ -> True
  vereinige    = vereinigeMT3
  schneide     = schneideMT3
  zieheab      = zieheabMT3
  istTeilmenge = istTeilmengeMT3
  zeige        = zeigeMT3

class Menge m => Relation m where
  istLeereRelation :: m -> Bool
  istLeereRelation = sindGleich leereMenge
  istAllRelation :: m -> Bool
  istAllRelation = sindGleich allMenge
  istLinkstotal :: m -> Bool
  istRechtstotal :: m -> Bool
  istReflexiv :: m -> Bool
  istSymmetrisch :: m -> Bool
  istTransitiv :: m -> Bool
  istQuasiOrdnung :: m -> Bool
  istQuasiOrdnung m = istReflexiv m && istTransitiv m
  istAequivalenzrelation :: m -> Bool
  istAequivalenzrelation m = istQuasiOrdnung m && istSymmetrisch m

allPairs :: [(Landeshauptstadt, Landeshauptstadt)]
allPairs = [(x, y) | x <- defaultValue, y <- defaultValue]

-- Überprüft, wenn es Paare (x, y) und (y, z) gibt, auch ein Paar (x, z) vorhanden sein muss
istTransitivStaedtepaar :: [Staedtepaar] -> [Staedtepaar] -> Bool
istTransitivStaedtepaar _  [] = True
istTransitivStaedtepaar orig ((l, l'):ps) =
  -- Schaue ob die Transitivitaet aller (l R l'')s gilt, dann schau weiter
  all (`elem` orig) l_l''s && (istTransitivStaedtepaar (orig)) ps
  where
    -- Alle vorgegebene (l' R x)s
    l'_l''s = filter (\pair -> fst pair == l') orig
    l''s    = map (snd) l'_l''s
    -- Alle zu pruefende Transitivitaeten aus l
    l_l''s  = [(l, l'') | l'' <- l''s]

-- (a)
instance Defaultable Staedtepaar where
    defaultValue = allPairs
data IstPartnerstadtVon1 = IPV1 (MT1 (Landeshauptstadt, Landeshauptstadt))


instance Menge IstPartnerstadtVon1 where
  leereMenge = IPV1(MT1 [])
  allMenge = IPV1(MT1 defaultValue)
  istMenge (IPV1 m) = istMengeMT1 m
  vereinige (IPV1 m1) (IPV1 m2) = IPV1 $ vereinigeMT1 m1 m2
  schneide (IPV1 m1) (IPV1 m2) = IPV1 $ schneideMT1 m1 m2
  zieheab (IPV1 m1) (IPV1 m2) = IPV1 $ zieheabMT1 m1 m2
  istTeilmenge (IPV1 m1) (IPV1 m2) = istTeilmengeMT1 m1 m2
  zeige (IPV1 m1) = zeigeMT1 m1

instance Relation IstPartnerstadtVon1 where
  istLinkstotal  = istLinkstotalIPV1
  istRechtstotal = istRechtstotalIPV1
  istReflexiv = istReflexivIPV1
  istSymmetrisch = istSymmetrischIPV1
  istTransitiv (IPV1 (MT1 paare)) = istTransitivStaedtepaar paare paare


-- Überprüft, ob für jede Landeshauptstadt mindestens ein Paar existiert, in dem sie der erste Eintrag ist.
istLinkstotalIPV1 :: IstPartnerstadtVon1 -> Bool
istLinkstotalIPV1 (IPV1 (MT1 paare)) = all (\x -> any (\(a, _) -> a == x) paare) defaultValue

-- Überprüft, ob für jede Landeshauptstadt mindestens ein Paar existiert, in dem sie der zweite Eintrag ist.
istRechtstotalIPV1 :: IstPartnerstadtVon1 -> Bool
istRechtstotalIPV1 (IPV1 (MT1 paare)) = all (\x -> any (\(_, a) -> a == x) paare) defaultValue

-- Überprüft, ob für jede Landeshauptstadt ein Paar (x,x) existiert.
istReflexivIPV1 :: IstPartnerstadtVon1 -> Bool
istReflexivIPV1 (IPV1 (MT1 paare)) = length allPaare == length allUnikalStaedte
  where
    allPaare = filter (\(x, y) -> x == y) paare
    allUnikalStaedte = nub $ map fst paare ++ map snd paare

-- Überprüft, ob für jedes Paar (x,y) auch das Paar (y,x) existiert.
istSymmetrischIPV1 :: IstPartnerstadtVon1 -> Bool
istSymmetrischIPV1 (IPV1 (MT1 paare)) = all (\(x, y) -> elem (y, x) paare) paare


-- (b)  
data IstPartnerstadtVon3 = IPV3 (MT3 (Landeshauptstadt, Landeshauptstadt))

instance Menge IstPartnerstadtVon3 where
  leereMenge   = IPV3 (MT3 (\_ -> False))
  allMenge     = IPV3 (MT3 (\_ -> True ))
  istMenge     = \_ -> True
  vereinige (IPV3 m1) (IPV3 m2) = IPV3 $ vereinigeMT3 m1 m2
  schneide (IPV3 m1) (IPV3 m2) = IPV3 $ schneideMT3 m1 m2
  zieheab (IPV3 m1) (IPV3 m2) = IPV3 $ zieheabMT3 m1 m2
  istTeilmenge (IPV3 m1) (IPV3 m2) = istTeilmengeMT3 m1 m2
  zeige (IPV3 m1) = zeigeMT3 m1

instance Relation IstPartnerstadtVon3 where
  istLinkstotal  = istLinkstotalIPV3
  istRechtstotal = istRechtstotalIPV3
  istReflexiv = istReflexivIPV3
  istSymmetrisch = istSymmetrischIPV3
  istTransitiv (IPV3 (MT3 predicate)) = istTransitivStaedtepaar allPairsMT3 allPairsMT3
    where allPairsMT3 = [(a, b) | a <- defaultValue, b <- defaultValue, predicate (a, b)]

-- Überprüft, ob für jede Landeshauptstadt mindestens ein Paar existiert, in dem sie der erste Eintrag ist.
istLinkstotalIPV3 :: IstPartnerstadtVon3 -> Bool
istLinkstotalIPV3 (IPV3 (MT3 predicate)) = all (\x -> any (\(a, _) -> a == x) allPairsMT3) defaultValue
  where allPairsMT3 = [(a, b) | a <- defaultValue, b <- defaultValue, predicate (a, b)]

-- Überprüft, ob für jede Landeshauptstadt ein Paar (x,x) existiert.
istRechtstotalIPV3 :: IstPartnerstadtVon3 -> Bool
istRechtstotalIPV3 (IPV3 (MT3 predicate)) = all (\x -> any (\(_, a) -> a == x) allPairsMT3) defaultValue
  where allPairsMT3 = [(a, b) | a <- defaultValue, b <- defaultValue, predicate (a, b)]

-- Überprüft, ob für jedes Paar (x,y) auch das Paar (y,x) existiert.
istReflexivIPV3 :: IstPartnerstadtVon3 -> Bool
istReflexivIPV3 (IPV3 (MT3 predicate)) =  length allPaare == length allUnikalStaedte
  where allPairsMT3 = [(a, b) | a <- defaultValue, b <- defaultValue, predicate (a, b)]
        allPaare = filter (\(x, y) -> x == y) allPairsMT3
        allUnikalStaedte = nub $ map fst allPairsMT3 ++ map snd allPairsMT3

-- Überprüft, ob für jedes Paar (x,y) auch das Paar (y,x) existiert.
istSymmetrischIPV3 :: IstPartnerstadtVon3 -> Bool
istSymmetrischIPV3 (IPV3 (MT3 predicate))  = all (\(x, y) -> elem (y, x) allPairsMT3) allPairsMT3
   where allPairsMT3 = [(a, b) | a <- defaultValue, b <- defaultValue, predicate (a, b)]


-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
--------------------------------------------------------------------------Aufgabenblatt 8 .hs--------------------------------------------------------------------------

--------------------------------------------------------------------------------- A.1 --------------------------------------------------------------------------

-- Protoimplementierungen für Klassenfunktionen

class Relation r => RelationPlus r where
    vereinigeR :: r -> r -> r
    schneideR :: r -> r -> r
    schneideR = error "nicht gewählt"
    komplementiereR :: r -> r
    komplementiereR = zieheab allMenge

--------------------------------------------------------------------------------- A.2 --------------------------------------------------------------------------
instance RelationPlus IstPartnerstadtVon1 where
    vereinigeR (IPV1 m1) (IPV1 m2) = IPV1 $ vereinigeMT1 m1 m2

instance RelationPlus IstPartnerstadtVon3 where
    vereinigeR (IPV3 m1) (IPV3 m2) = IPV3 $ vereinigeMT3 m1 m2

--------------------------------------------------------------------------------- A.3 --------------------------------------------------------------------------
type Coordinate = (Int, Int)
data Baumhain = Rectangle { x :: Int, y :: Int, lengthX :: Int, widthY :: Int } deriving (Show, Eq) -- die Figur auf dem Spielfeld ist wie ein Schiff im Spiel Seeschlacht
data Orientation = Links | Rechts | Nicht deriving (Show, Eq) -- Ausrichtung der Figur auf dem Spielfeld z.B. 2x4 - Links, 4x2 - Rechts                   
type Field = [[Int]] -- ein Spielfeld, in dem alle "Zellen", die von einer Spielfigur besetzt werden, 1 sind, die freien Zellen 0 sind und Züge - 2, und Figur 1x1 - 3

maxForestArea = 19 -- maximale Anzahl der belegten Zellen auf dem Spielfeld
sizeFiguren = [(2,4),(1,3),(1,3),(2,2),(1,1)] -- alle Arten von Spielfiguren
existingBaumhaine = []

-- Funktion zur Eingabe der Größe des Weihnachtswaldgitters.  Die Größe muss >= 10 sein.
inputFieldSize :: IO Int
inputFieldSize = do
    putStrLn "Geben Sie einen Wert für Weihnachtswaldgittergröße: "
    input <- getLine
    let size = convertStringToInt input
    if size < 10 then do -- Rekursive Aufforderung, falls die Eingabe ungültig ist
        putStrLn "Ungültige Größe.  Die Größe muss mindestens 10 betragen."
        inputFieldSize
    else return size


-- Erzeugt ein zweidimensionales Spielfeld Field aus 0, 1 und 3.
--               0: Leere Zelle
--               1: Zelle belegt durch ein Rechteck (größer als 1x1)
--               3: Zelle belegt durch ein 1x1 Rechteck

createField :: [Baumhain] -> Int -> Field
createField baumhaine n =
    let field = replicate n (replicate n 0) -- Erzeuge ein mit Nullen gefülltes Spielfeld
    in foldl (addBaumhain) field baumhaine  -- Füge die Rechtecke nacheinander hinzu

addBaumhain :: Field -> Baumhain -> Field
addBaumhain field rect =
    let xCoord = x rect
        yCoord = y rect
        lenX = lengthX rect
        lenY = widthY rect
    in  myMapWithIndex (\rowIndex row -> -- Iteriere über die Zeilen
            myMapWithIndex (\colIndex cell -> -- Iteriere über die Spalten
                if colIndex >= xCoord && colIndex < xCoord + lenX && -- Überprüfe, ob die Koordinaten innerhalb des Rechtecks liegen
                    rowIndex >= yCoord && rowIndex < yCoord + lenY then 
                        if lenX == 1 && lenY == 1 then 3
                        else 1 -- 3 für 1x1 Rechtecke, sonst 1
                else cell -- Zelle unverändert lassen
            ) row
        ) field

-- Custom recursive function to mimic MapWithIndex for lists of lists
myMapWithIndex :: (Int -> a -> b) -> [a] -> [b]
myMapWithIndex f [] = []
myMapWithIndex f (x:xs) = f (0) x : myMapWithIndex (\i y -> f (i+1) y) xs



-- Simuliert einen Spielzug auf dem Spielfeld.
zug :: Coordinate -> Field -> IO Field
zug (x,y) field = 
    if x >= 0 && x < length field && y >= 0 && y < length (head field) then do -- Überprüfe die Gültigkeit der Koordinaten
        let elem = getElement field x y -- Hole den Wert der Zelle an den gegebenen Koordinaten
        case elem of
            0 -> do
                putStrLn "Mißerfolg!"
                let newField = updateField x y 2 field
                return newField
            1 -> do
                putStrLn "\nSie haben es geschafft, den Weihnachtsbaum auszublasen"
                let newField = updateField x y 2 field
                return newField
            2 -> do 
                putStrLn "Bitte versuchen Sie es erneut.\nSie haben bereits versucht, den Weihnachtsbaum in dieser Zelle auszublasen"
                return field
            3 -> do 
                let buergermeister = checkBuergermeister field
                case buergermeister of
                    True -> putStrLn "\n!!!! Sie haben Respektlosigkeit gezeigt !!!!\n!!!! Das gegnerische Team hat gewonnen !!!!"
                    False -> putStrLn "\n!!!! Herzlichen Glückwunsch !!!!\n!!!! Sie haben den Wettbewerb gewonnen !!!!"
                return [[]]  -- Leeres Feld zurückgeben, da das Spiel beendet ist
    else do
        putStrLn "Ungültiger Coordinate. Bitte versuchen Sie es erneut."
        return field -- Unverändertes Feld zurückgeben bei ungültigen Koordinaten

-- Überprüft, ob im gegebenen Spielfeld noch mindestens ein Weihnachtsbaumhain größer als 1x1 vorhanden ist.
checkBuergermeister :: Field -> Bool
checkBuergermeister field = any (elem 1) field
    
-- Aktualisiert ein Element in einem zweidimensionalen Feld (Field).
updateField :: Int -> Int -> Int -> Field -> Field
updateField row col newElement field =
    myMapWithIndex (\rowIndex r ->
        if rowIndex == row then 
            myMapWithIndex (\colIndex x -> if colIndex == col then newElement else x) r
        else r
    ) field

-- Zugang zum Zellen auf dem Spielfeld
getElement :: Field -> Int -> Int -> Int
getElement field x y = field !! y !! x

-- Gibt ein zweidimensionales Integer-Feld ([[Int]]) formatiert auf der Konsole aus.
prettyPrintField :: [[Int]] -> IO ()
prettyPrintField field = do
    let revField = reverse field
    putStrLn "+---+"
    mapM_ (\row -> -- Iteriere über jede Zeile des gespiegelten Felds
        putStrLn $ "+ " ++ unwords (map show row) ++ " +" -- Füge Begrenzungen links und rechts hinzu und formatiere die Zahlen
        )revField
    putStrLn "+---+"

-- Funktion zur Eingabe des Startpunkts für einen Baumhain.
inputStartPunktBaumhain :: IO Coordinate
inputStartPunktBaumhain = do
    putStrLn "Geben Sie den Startpunkt (unten links z.B. (0,0)): "
    input <- getLine
    let startpunkt = convertStringToCoordinates input
    if startpunkt == (-1,-1) then do -- Fehlerbehandlung für ungültige Eingabe
        putStrLn "Ungültiger Baumhain Startpunkt. Bitte versuchen Sie es erneut."
        inputStartPunktBaumhain
    else return startpunkt


-- Funktion zur Eingabe der Ausrichtung (Links oder Rechts) eines Baumhains.
inputSizeBaumhain :: Int -> Int -> IO Orientation
inputSizeBaumhain x y = do
    putStrLn $ "Geben Sie die Position " ++ show x ++"x"++ show y ++ " - Links, "++ show y ++ "x"++ show x ++" - Rechts fest: "
    input <- getLine
    let position = convertStringToOrientation input
    if position == Nicht then do -- Fehlerbehandlung für ungültige Eingabe
        putStrLn "Ungültige Baumhain Position. Bitte versuchen Sie es erneut."
        inputSizeBaumhain x y
    else return position


-- Funktion zur Umwandlung eines Strings in einen Integer.  Gibt 0 zurück, falls die Umwandlung fehlschlägt.
convertStringToInt :: String -> Int
convertStringToInt str =
  case reads str of
    [(i, "")] -> i -- Erfolgreiche Umwandlung: Der String wurde vollständig in einen Integer (i) umgewandelt.  Der Reststring ist leer ("").
    _ -> 0       -- Fehlgeschlagene Umwandlung: Entweder kein Integer im String oder der String enthielt zusätzliche Zeichen nach dem Integer.  In diesem Fall wird 0 zurückgegeben.

-- Funktion zur Umwandlung eines Strings in Koordinaten.
convertStringToCoordinates :: String -> Coordinate
convertStringToCoordinates []  = (-1, -1)
convertStringToCoordinates str =
    let
        parenRemoved = drop 1 $ init str  -- Klammern entfernen
        parts = break (== ',') parenRemoved -- String an Komma trennen
        xStr = fst parts -- x-Koordinate extrahieren
        yStr = drop 1 $ snd parts -- y-Koordinate extrahieren
        safeRead :: String -> Maybe Int -- Sichere Umwandlung in Integer
        safeRead s =
            case reads s of
                [(x, "")] -> Just x
                _ -> Nothing
    in case (safeRead xStr, safeRead yStr) of
        (Just x, Just y) -> (x, y)
        _ -> (-1, -1) -- Fehlerwert bei ungültiger Eingabe


-- Funktion zur Umwandlung eines Strings in eine Ausrichtung.
convertStringToOrientation :: String -> Orientation
convertStringToOrientation s 
    | s == "Links"      = Links
    | s == "Rechts"     = Rechts
    | otherwise         = Nicht


-- Funktion zum Erstellen eines Baumhain-Rechtecks.
createRectangle :: Coordinate -> Orientation -> Coordinate -> Baumhain
createRectangle (x, y) Links (l, w) = Rectangle {x = x, y = y, lengthX = l, widthY = w}
createRectangle (x, y) Rechts (l, w) = Rectangle {x = x, y = y, lengthX = w, widthY = l}


inputSpielfiguren :: Int -> [Baumhain] ->[Coordinate] -> IO [Baumhain]
inputSpielfiguren n existingBaumhaine [] = return existingBaumhaine
inputSpielfiguren n existingBaumhaine (x:xs) = do
  baumhaine <- processBaumhain n existingBaumhaine x
  if baumhaine == [Rectangle { x = 0, y = 0, lengthX = 0, widthY = 0 }] then do 
    inputSpielfiguren n existingBaumhaine (x:xs)
  else do
    inputSpielfiguren n baumhaine xs
  


processBaumhain :: Int -> [Baumhain] -> Coordinate -> IO [Baumhain]
processBaumhain n existingBaumhaine (l, w) = do
    putStrLn $ "Koordinaten für Baumhain " ++ show l ++ "x" ++ show w ++ " : "
    startpunkt <- inputStartPunktBaumhain
    position <- inputSizeBaumhain l w
    let baumhain = createRectangle startpunkt position (l, w)
    --putStrLn $ show existingBaumhaine
    if checkBaumhain baumhain existingBaumhaine n then do
        return $ existingBaumhaine ++ [baumhain]
    else do
        putStrLn "Es ist unmöglich, diese Baumhain zu pflanzen! Bitte versuchen Sie es erneut."
        return [Rectangle { x = 0, y = 0, lengthX = 0, widthY = 0 }]


-- Überprüft, ob sich zwei Rechtecke überschneiden.
-- r1, r2: Die zu vergleichenden Rechtecke.
-- Rückgabewert: True, wenn sich die Rechtecke überschneiden, False sonst.
intersects :: Baumhain -> Baumhain -> Bool
intersects r1 r2 =
  xOverlap && yOverlap
  where
    -- Berechnung der minimalen und maximalen x-Koordinaten für Rechteck 1
    r1xMin = x r1
    r1xMax = x r1 + lengthX r1
    -- Berechnung der minimalen und maximalen y-Koordinaten für Rechteck 1
    r1yMin = y r1
    r1yMax = y r1 + widthY r1
    -- Berechnung der minimalen und maximalen x-Koordinaten für Rechteck 2
    r2xMin = x r2
    r2xMax = x r2 + lengthX r2
    -- Berechnung der minimalen und maximalen y-Koordinaten für Rechteck 2
    r2yMin = y r2
    r2yMax = y r2 + widthY r2
    -- Überlappung in x-Richtung:  Es gibt keine Überlappung, wenn das Maximum des einen Rechtecks kleiner als das Minimum des anderen ist.
    xOverlap = not (r1xMax < r2xMin || r2xMax < r1xMin)
    -- Überlappung in y-Richtung:  Es gibt keine Überlappung, wenn das Maximum des einen Rechtecks kleiner als das Minimum des anderen ist.
    yOverlap = not (r1yMax < r2yMin || r2yMax < r1yMin)


-- Überprüft, ob ein Rechteck auf einem Spielfeld platziert werden kann, ohne andere Rechtecke zu überschneiden.
-- rect: Das zu platzierende Rechteck.
-- rectangles: Liste der bereits platzierten Rechtecke.
-- n: Die Größe des Spielfelds (n x n).
-- Rückgabewert: True, wenn das Rechteck platziert werden kann, False sonst.
checkBaumhain :: Baumhain -> [Baumhain] -> Int -> Bool
checkBaumhain rect rectangles n =
  inBounds && notIntersects
  where
    -- Überprüft, ob das Rechteck innerhalb der Spielfeldgrenzen liegt.
    inBounds =  0 <= x rect && x rect + lengthX rect <= n && 0 <= y rect && y rect + widthY rect <= n
    -- Überprüft, ob das Rechteck keines der bereits platzierten Rechtecke überschneidet.
    notIntersects = all (not . intersects rect) rectangles

------------------------------------------------------------------------------ Tests ---------------------------------------------------------------------------------

main = do
    n <- inputFieldSize
    putStrLn $ "Weihnachtswaldgittergröße ist " ++ show n
    putStrLn ""
    baumhaine1 <- inputSpielfiguren n existingBaumhaine sizeFiguren
    putStrLn $ "Baumhaine №1 sind " ++ show baumhaine1
    let field1 = createField baumhaine1 n
    putStrLn ""
    prettyPrintField field1
    putStrLn ""

    baumhaine2 <- inputSpielfiguren n existingBaumhaine sizeFiguren
    putStrLn $ "Baumhaine №2 sind " ++ show baumhaine2
    let field2 = createField baumhaine2 n
    putStrLn ""
    prettyPrintField field2

    putStrLn "-----------------------------------------------------A.2-----------------------------------------------------"
    putStrLn ""
    let bIPV1 = IPV1 (MT1 [(B,B)])
        eIPV1 = IPV1 (MT1 [(E,E)])
    putStrLn $ "vereinigeR  {} {(B,B)}: " ++ (zeige . vereinigeR leereMenge $ bIPV1)
    putStrLn $ "vereinigeR {(B,B)} {(B,B)}: " ++ (zeige $ vereinigeR (bIPV1) (bIPV1))
    putStrLn $ "vereinigeR {(B,B)} {(E,E)}: " ++ (zeige $ vereinigeR (bIPV1) (eIPV1))
    putStrLn ""
    putStrLn $ "komplementiereR . zieheab allMenge $ {(B,B)}: " ++ (zeige . komplementiereR . zieheab allMenge $ bIPV1)
    putStrLn ""