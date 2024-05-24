import Data.Time.Clock
import Data.Time.LocalTime
import Data.List
import System.IO
import System.Directory (doesFileExist)

-- Definición del tipo de datos para representar la información de un vehículo
data Vehiculo = Vehiculo {
    placa :: String,
    entrada :: LocalTime,
    salida :: Maybe LocalTime  -- Usamos Maybe para representar que el vehículo aún está en el parqueadero o ya salió
} deriving (Show, Read)

-- Función para obtener la fecha y hora actuales en hora local
obtenerFechaYHoraActual :: IO LocalTime
obtenerFechaYHoraActual = do
    tiempoUTC <- getCurrentTime
    zonaHoraria <- getTimeZone tiempoUTC
    return $ utcToLocalTime zonaHoraria tiempoUTC

-- Función para registrar la entrada de un vehículo al parqueadero
registrarEntrada :: String -> LocalTime -> [Vehiculo] -> [Vehiculo]
registrarEntrada placaVehiculo tiempo parqueadero =
    Vehiculo placaVehiculo tiempo Nothing : parqueadero

-- Función para registrar la salida de un vehículo del parqueadero
registrarSalida :: String -> LocalTime -> [Vehiculo] -> [Vehiculo]
registrarSalida placaVehiculo tiempo parqueadero =
    map (\v -> if placaVehiculo == placa v then v { salida = Just tiempo } else v) parqueadero

-- Función para buscar un vehículo por su placa en el parqueadero
buscarVehiculo :: String -> [Vehiculo] -> Maybe Vehiculo
buscarVehiculo placaVehiculo parqueadero =
    find (\v -> placaVehiculo == placa v && isNothing (salida v)) parqueadero
    where
        isNothing Nothing = True
        isNothing _       = False

-- Función para calcular el tiempo que un vehículo permaneció en el parqueadero
tiempoEnParqueadero :: Vehiculo -> LocalTime -> NominalDiffTime
tiempoEnParqueadero vehiculo tiempoActual =
    diffUTCTime (localTimeToUTC (hoursToTimeZone 0) tiempoActual) (localTimeToUTC (hoursToTimeZone 0) (entrada vehiculo))

-- Función para guardar la información de los vehículos en un archivo de texto
guardarParqueadero :: [Vehiculo] -> IO ()
guardarParqueadero parqueadero = withFile "parqueadero.txt" WriteMode $ \h -> do
    hPutStr h (unlines (map mostrarVehiculo parqueadero))
    putStrLn "Parqueadero guardado en el archivo parqueadero.txt."

-- Función para cargar la información de los vehículos desde un archivo de texto
cargarParqueadero :: IO [Vehiculo]
cargarParqueadero = do
    archivoExiste <- doesFileExist "parqueadero.txt"
    if archivoExiste
        then withFile "parqueadero.txt" ReadMode $ \h -> do
            contenido <- hGetContents h
            let lineas = lines contenido
                vehiculos = map leerVehiculo lineas
            length vehiculos `seq` return vehiculos  -- Forzamos la evaluación de 'vehiculos' antes de cerrar el handle
        else return []  -- Si el archivo no existe, devolvemos una lista vacía
    where
        leerVehiculo linea = read linea :: Vehiculo

-- Función para mostrar la información de un vehículo como cadena de texto
mostrarVehiculo :: Vehiculo -> String
mostrarVehiculo vehiculo =
    placa vehiculo ++ "," ++ show (entrada vehiculo) ++ "," ++ show (salida vehiculo)

-- Función para listar los vehículos en el parqueadero
listarVehiculos :: [Vehiculo] -> IO ()
listarVehiculos parqueadero = do
    let vehiculosEnParqueadero = filter (isNothing . salida) parqueadero
    if null vehiculosEnParqueadero
        then putStrLn "No hay vehículos en el parqueadero."
        else mapM_ (putStrLn . mostrarVehiculo) vehiculosEnParqueadero
    where
        isNothing Nothing = True
        isNothing _       = False

-- Función principal del programa
main :: IO ()
main = do
    -- Cargar el parqueadero desde el archivo de texto
    parqueadero <- cargarParqueadero
    putStrLn "¡Bienvenido al Sistema de Gestión de Parqueadero!"

    -- Ciclo principal del programa
    cicloPrincipal parqueadero

-- Función para el ciclo principal del programa
cicloPrincipal :: [Vehiculo] -> IO ()
cicloPrincipal parqueadero = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar entrada de vehículo"
    putStrLn "2. Registrar salida de vehículo"
    putStrLn "3. Buscar vehículo por placa"
    putStrLn "4. Listar vehiculos"
    putStrLn "5. Salir"
    opcion <- getLine

    case opcion of
        "1" -> do
            putStrLn "Ingrese la placa del vehículo:"
            placaVehiculo <- getLine
            tiempoActual <- obtenerFechaYHoraActual
            let parqueaderoActualizado = registrarEntrada placaVehiculo tiempoActual parqueadero
            putStrLn $ "Vehículo con placa " ++ placaVehiculo ++ " ingresado al parqueadero."
            guardarParqueadero (reverse parqueaderoActualizado)
            cicloPrincipal parqueaderoActualizado

        "2" -> do
            putStrLn "Ingrese la placa del vehículo a salir:"
            placaVehiculo <- getLine
            let vehiculoEncontrado = buscarVehiculo placaVehiculo parqueadero
            case vehiculoEncontrado of
                Just _ -> do
                    tiempoActual <- obtenerFechaYHoraActual
                    let parqueaderoActualizado = registrarSalida placaVehiculo tiempoActual parqueadero
                    putStrLn $ "Vehículo con placa " ++ placaVehiculo ++ " salido del parqueadero."
                    guardarParqueadero (reverse parqueaderoActualizado)
                    cicloPrincipal parqueaderoActualizado
                Nothing -> do
                    putStrLn $ "No se encontró el vehículo con placa " ++ placaVehiculo ++ " en el parqueadero."
                    cicloPrincipal parqueadero

        "3" -> do
            putStrLn "Ingrese la placa del vehículo a buscar:"
            placaVehiculo <- getLine
            case buscarVehiculo placaVehiculo parqueadero of
                Just vehiculo -> do
                    tiempoActual <- obtenerFechaYHoraActual
                    let tiempoTotal = tiempoEnParqueadero vehiculo tiempoActual
                    putStrLn $ "El vehículo con placa " ++ placaVehiculo ++ " se encuentra en el parqueadero."
                    putStrLn $ "Tiempo en parqueadero: " ++ show tiempoTotal ++ " segundos."
                Nothing -> putStrLn "Vehículo no encontrado en el parqueadero."
            cicloPrincipal parqueadero

        "4" -> do
            putStrLn "Los vehículos listados son:"
            listarVehiculos parqueadero
            cicloPrincipal parqueadero

        "5" -> putStrLn "Hasta pronto."

        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            cicloPrincipal parqueadero