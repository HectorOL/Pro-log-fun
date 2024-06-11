--Primer Ejericio
import Data.List (isInfixOf)
import Data.List (permutations)

libros = [
    ("Cien años de soledad", 432, ["Gabriel García Márquez"], "Editorial Sudamericana"),
    ("El amor en los tiempos del cólera", 368, ["Gabriel García Márquez"], "EU Alfaguara"),
    ("Harry Potter y la piedra filosofal", 256, ["J.K. Rowling"], "Salamandra"),
    ("El señor de los anillos: La comunidad del anillo", 480, ["J.R.R. Tolkien"], "EU Minotauro"),
    ("El código Da Vinci", 592, ["Dan Brown"], "Planeta"),
    ("Orgullo y prejuicio", 96, ["Jane Austen"], "Penguin Clásicos"),
    ("La ladrona de libros", 544, ["Markus Zusak"], "Debolsillo"),
    ("1984", 368, ["George Orwell"], "Debolsillo"),
    ("Los pilares de la tierra", 1200, ["Ken Follett"], "Plaza & Janés"),
    ("La chica del tren", 448, ["Paula Hawkins"], "Planeta"),
    ("Los juegos del hambre", 432, ["Suzanne Collins"], "Molino"),
    ("El alquimista", 99, ["Paulo Coelho"], "Planeta"),
    ("Matar a un ruiseñor", 424, ["Harper Lee"], "HarperCollins"),
    ("Buenos presagios", 416, ["Neil Gaiman", "Terry Pratchett"], "Debolsillo"),
    ("El símbolo perdido", 672, ["Dan Brown"], "Planeta"),
    ("Sapiens: De animales a dioses", 496, ["Yuval Noah Harari"], "Debate"),
    ("El hombre que calculaba", 256, ["Malba Tahan", "Breno de Alencar Bianco"], "Ediciones EDAF"),
    ("El último deseo", 320, ["Andrzej Sapkowski"], "Alamut"),
    ("La historia interminable", 432, ["Michael Ende"], "Alfaguara"),
    ("El gran diseño", 208, ["Stephen Hawking", "Leonard Mlodinow"], "EU Crítica")]

dosAutores lista = [titulo | (titulo, _, autores, _) <- lista, length (autores) >= 2]

editorialEU lista = [titulo | (titulo, _, _, editorial) <- lista, take 2 editorial == "EU"]

autores15 lista = [autores | (titulo, _, autores, _) <- lista, length (titulo) >= 15]

eliminarRepetidos [] = []
eliminarRepetidos (x:subcadena) = x : eliminarRepetidos (filter (/= x) subcadena)

cantidadEditorial lista = eliminarRepetidos [(editorial, length [titulo | (_, _, _, editorial') <- lista, editorial' == editorial]) | (titulo, _, _, editorial) <- lista]

menosCienPag lista = [titulo | (titulo, paginas, _, _) <- lista, paginas < 100]

--Segundo Ejercicio
nDigitos lista n = [numero | numero <- lista, length (show numero) >= n]

--Tercer Ejercicio
patronNombres lista p = [nombre | nombre <- lista, isInfixOf p nombre]

--Cuarto Ejercicio
esAscendente [] = True
esAscendente (p:[]) = True
esAscendente (p:r) = p <= head r && esAscendente r

listaAscendente lista = [numero | numero <- lista, esAscendente numero]

--Quinto Ejercicio

dividirAscendentes [] = []
dividirAscendentes lista = ascendente : dividirAscendentes resto
  where
    (ascendente, resto) = extraerAscendentes lista

extraerAscendentes [] = ([], [])
extraerAscendentes [x] = ([x], [])
extraerAscendentes (x:y:xs)
  | y >= x    = let (ascendentes, resto) = extraerAscendentes (y:xs)
                in (x:ascendentes, resto)
  | otherwise = ([x], y:xs)

listaAlzas l = [(length dias - 1) | dias <- dividirAscendentes l]

--Sexto Ejercicio
noLetras lista = [nLetras cadena | cadena <- lista]
  where
    nLetras cadena = length [letra | letra <- cadena, not (elem letra ['A'..'z'])]
 
--Septimo Ejercicio
valorMenor lista = [menor tupla | tupla <- lista]
  where
    menor (x, y) | x < y = x
                 | y < x = y
                 | otherwise = 0

--Octavo Ejercicio
menor5 lista = [palabra | palabra <- lista, length palabra < 5]

--Noveno Ejercicio
palabrasConTodasLasLetras :: String -> [String]
palabrasConTodasLasLetras palabra = filter (\x -> length x == length palabra) $ concatMap permutations (subcadenas palabra)

subcadenas :: String -> [String]
subcadenas [] = [[]]
subcadenas (x:xs) = subcadenas xs ++ map (x:) (subcadenas xs)

--Decimo Ejercicio
personas = [
    ("Juan Perez", 32, "Soltero", "M"),
    ("Maria Garcia", 28, "Casada", "F"),
    ("Pedro Lopez", 45, "Divorciado", "M"),
    ("Ana Sanchez", 21, "Soltera", "F"),
    ("Carlos Gomez", 50, "Casado", "M"),
    ("Isabel Rodriguez", 37, "Divorciada", "F"),
    ("David Martinez", 17, "Soltero", "M"),
    ("Laura Fernandez", 25, "Casada", "F"),
    ("Pablo Gonzalez", 42, "Divorciado", "M"),
    ("Sandra Ruiz", 30, "Soltera", "F"),
    ("Marcos Alonso", 23, "Soltero", "M"),
    ("Elena Lopez", 48, "Casada", "F"),
    ("Diego Sanchez", 35, "Divorciado", "M"),
    ("Patricia Jimenez", 26, "Soltera", "F")]

nombreEstado lista = [(nombre, estado) | (nombre, _, estado, _) <- lista]

divorciados lista = [nombre | (nombre, _, estado, _) <- lista, estado == "Divorciado" || estado == "Divorciada"]

mayorEdadSoltera lista = length [nombre | (nombre, edad, estado, sexo) <- lista, estado == "Soltera", edad >= 18, sexo == "F"]

mayorEdadCasado lista = length [nombre | (nombre, edad, estado, sexo) <- lista, estado == "Casado", edad >= 18, sexo == "M"]

totalHombres lista = length [nombre | (nombre, _, _, sexo) <- lista, sexo == "M"]

proporcion lista = if totalHombres lista == 0 then 0 else ((fromIntegral $ mayorEdadCasado lista) / (fromIntegral $ totalHombres lista))

menoresEdad lista = [(nombre, edad, estado, sexo) | (nombre, edad, estado,sexo) <- lista, edad < 18]



