figura1 = Rectangulo 2 3
figura2 = Circulo 4
figura3 = Triangulo 3 4
figura4 = Rectangulo 3 3
listaFiguras = Lista [figura1, figura2, figura3]
--Ejercicio 1
data Figura = Rectangulo Float Float| Circulo Float | Triangulo Float Float deriving (Show, Eq, Ord)

--Ejercicio 2
getArea :: Figura -> Float
getArea (Rectangulo b h) = b * h
getArea (Circulo r) = pi * r^2
getArea (Triangulo b h) = (b * h) / 2

--Ejercicio 3
data ListaFiguras = Lista [Figura] deriving (Show, Eq, Ord)

anadeFinal :: Figura -> ListaFiguras -> ListaFiguras
anadeFinal f (Lista l) = Lista (l ++ [f])

anadeInicio :: Figura -> ListaFiguras -> ListaFiguras
anadeInicio f (Lista l) = Lista (f : l)

getCuadrados :: ListaFiguras -> ListaFiguras
getCuadrados (Lista l) = Lista [x | x <- l, esCuadrado x]
    where esCuadrado (Rectangulo b h) = b == h
          esCuadrado _ = False

getTriangulos :: ListaFiguras -> ListaFiguras
getTriangulos (Lista l) = Lista [x | x <- l, esTriangulo x]
    where esTriangulo (Triangulo _ _) = True
          esTriangulo _ = False

getCirculos :: ListaFiguras -> ListaFiguras
getCirculos (Lista l) = Lista [x | x <- l, esCirculo x]
    where esCirculo (Circulo _) = True
          esCirculo _ = False

getAreas :: ListaFiguras -> Float
getAreas (Lista l) = sum [getArea x | x <- l]




--Ejercicio 4
grid = createGrid 5 5 12
robot = createRobot (1, 2) West

type Position = (Int, Int)
data Orientation = North | South | East | West deriving (Show, Eq)
data Segment = Thin | Thick deriving (Show, Eq)
data Robot = Robot { position :: Position, orientation :: Orientation, time :: Int } deriving (Show)

-- Representación de la malla
type Grid = [[Segment]]

-- Crear una malla de tamaño n x m con al menos numThick tramos gruesos
createGrid :: Int -> Int -> Int -> Grid
createGrid n m numThick = replicate n (replicate m Thin) -- Inicialmente, todos los tramos son delgados

-- Cambiar un tramo de la malla a grueso en una posición específica
setThick :: Position -> Grid -> Grid
setThick (i, j) grid = take i grid ++ [take j (grid !! i) ++ [Thick] ++ drop (j+1) (grid !! i)] ++ drop (i+1) grid

-- Crear un robot en una posición determinada con tiempo de recorrido 0
createRobot :: Position -> Orientation -> Robot
createRobot pos ori = Robot pos ori 0

-- Función para obtener la posición actual del robot
getCurrentPosition :: Robot -> Position
getCurrentPosition robot = position robot

-- Función para girar el robot en una dirección válida
turnRobot :: Orientation -> Robot -> Robot
turnRobot newOrientation robot = robot { orientation = newOrientation, time = time robot + 5 }

-- Función para recorrer un segmento de la malla y actualizar el estado del robot
moveRobot :: Robot -> Segment -> Robot
moveRobot robot segment
    | segment == Thin = case orientation robot of
                          North -> robot { position = (fst (position robot), snd (position robot) + 1), time = time robot + 2 }
                          South -> robot { position = (fst (position robot), snd (position robot) - 1), time = time robot + 2 }
                          East  -> robot { position = (fst (position robot) + 1, snd (position robot)), time = time robot + 2 }
                          West  -> robot { position = (fst (position robot) - 1, snd (position robot)), time = time robot + 2 }
    | segment == Thick = case orientation robot of
                          North -> robot { position = (fst (position robot), snd (position robot) + 1), time = time robot + 1 }
                          South -> robot { position = (fst (position robot), snd (position robot) - 1), time = time robot + 1 }
                          East  -> robot { position = (fst (position robot) + 1, snd (position robot)), time = time robot + 1 }
                          West  -> robot { position = (fst (position robot) - 1, snd (position robot)), time = time robot + 1 }

-- Función para calcular la distancia entre dos posiciones
distance :: Position -> Position -> Int
distance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

-- Función para guiar al robot desde una posición inicial hacia una posición final
guideRobot :: Position -> Orientation -> Position -> Int -> [((Position, Orientation), Int)]
guideRobot startPos startOri endPos time
    | startPos == endPos = [((startPos, startOri), time)]
    | otherwise = do
        let orientations = [North, South, East, West]
        (newPos, newOri, newTime) <- [(nextPos, ori, time + 1) | ori <- orientations, let nextPos = move startPos ori, isValidMove nextPos, newTime <- [time + if ori == startOri then 0 else 5]]
        let results = guideRobot newPos newOri endPos newTime
        ((startPos, startOri), time) : results

-- Función para mover una posición en una dirección dada
move :: Position -> Orientation -> Position
move (x, y) North = (x, y + 1)
move (x, y) South = (x, y - 1)
move (x, y) East = (x + 1, y)
move (x, y) West = (x - 1, y)

-- Función para verificar si una posición es válida en la malla
isValidMove :: Position -> Bool
isValidMove (x, y) = x >= 1 && x <= 5 && y >= 1 && y <= 5
