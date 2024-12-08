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

--------------------------------------------------------------------------------- A.2 --------------------------------------------------------------------------
type Coordinate = (Int, Int)
data Baumhain = Rectangle { x :: Int, y :: Int, lengthX :: Int, widthY :: Int } deriving (Show) -- die Figur auf dem Spielfeld ist wie ein Schiff im Spiel Seeschlacht
data Orientation = Links | Rechts | Nicht deriving (Show, Eq) -- Ausrichtung der Figur auf dem Spielfeld z.B. 2x4 - Links, 4x2 - Rechts                   
type Field = [[Int]] -- ein Spielfeld, in dem alle "Zellen", die von einer Spielfigur besetzt werden, 1 sind, die freien Zellen 0 sind und Züge - 2

maxForestArea = 19 -- maximale Anzahl der belegten Zellen auf dem Spielfeld
sizeFiguren = [(2,4),(1,3),(1,3),(2,2),(1,1)] -- alle Arten von Spielfiguren


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
inputSizeBaumhain :: IO Orientation
inputSizeBaumhain = do
    putStrLn "Geben Sie die Position 2x4 - Links, 4x2 - Rechts fest: "
    input <- getLine
    let position = convertStringToOrientation input
    if position == Nicht then do -- Fehlerbehandlung für ungültige Eingabe
        putStrLn "Ungültige Baumhain Position. Bitte versuchen Sie es erneut."
        inputSizeBaumhain
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


-- Funktion zur Eingabe der Spielfiguren (Baumhaine).
inputSpielfiguren :: Int -> [Coordinate] -> IO [Baumhain]
inputSpielfiguren n coords = mapM (processBaumhain n) coords


-- Hilfsfunktion zum Verarbeiten eines einzelnen Baumhains.
processBaumhain :: Int -> Coordinate -> IO Baumhain
processBaumhain n (l, w) = do
    putStrLn $ "Koordinaten für Baumhain " ++ show l ++ "x" ++ show w ++ " : "
    startpunkt <- inputStartPunktBaumhain
    position <- inputSizeBaumhain
    let baumhain = createRectangle startpunkt position (l, w)
    if checkBaumhain n baumhain then return baumhain else do --Überprüfung ob Platzierung möglich ist
        putStrLn "Es ist unmöglich, diese Baumhain zu pflanzen! Bitte versuchen Sie es erneut."
        processBaumhain n (l, w)

-- Funktion zur Überprüfung der Baumhain-Platzierung. 
checkBaumhain :: Int -> Baumhain -> Bool
checkBaumhain n x = True  -- TODO:  Hier muss die eigentliche Überprüfungslogik implementiert werden!

------------------------------------------------------------------------------ Tests ---------------------------------------------------------------------------------

main = do
    n <- inputFieldSize
    putStrLn $ "Weihnachtswaldgittergröße ist " ++ show n
    baumhaine <- inputSpielfiguren n sizeFiguren
    putStrLn $ "Baumhaine sind " ++ show baumhaine
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