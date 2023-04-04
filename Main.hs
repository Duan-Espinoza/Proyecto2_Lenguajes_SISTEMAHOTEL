import System.IO

-- Definición del tipo de datos para las habitaciones
data Habitacion = Habitacion {
    tipo :: String,
    cantidad :: Int,
    tarifa :: Float
} deriving (Show)

-- Definición del tipo de datos para las reservas
data Reserva = Reserva {
    nombre :: String,
    habitacion :: Habitacion,
    checkIn :: String,
    checkOut :: String
} deriving (Show)

-- Definición del tipo de datos para el hotel
data Hotel = Hotel {
    nombreHotel :: String,
    habitaciones :: [Habitacion],
    reservas :: [Reserva]
} deriving (Show)

-- Función para mostrar el menú principal
menuPrincipal :: Hotel -> IO ()
menuPrincipal hotel = do
    putStrLn "Bienvenido al sistema de gestión hotelera"
    putStrLn "Seleccione una opción:"
    putStrLn "1. Opciones Administrativas"
    putStrLn "2. Opciones Generales"
    putStrLn "3. Salir"
    opcion <- getLine
    case opcion of
        "1" -> menuAdministrativo hotel
        "2" -> menuGeneral hotel
        "3" -> putStrLn "¡Hasta pronto!"
        _   -> do
            putStrLn "Opción inválida"
            menuPrincipal hotel

-- Función para mostrar el menú de opciones administrativas
menuAdministrativo :: Hotel -> IO ()
menuAdministrativo hotel = do
    putStrLn "Seleccione una opción administrativa:"
    putStrLn "1. Información de hotel"
    putStrLn "2. Cargar tipo de habitaciones"
    putStrLn "3. Asignar cantidad de habitaciones por tipo"
    putStrLn "4. Carga de tarifas"
    putStrLn "5. Consultar reservaciones"
    putStrLn "6. Estadísticas de ocupación"
    putStrLn "7. Regresar al menú principal"
    opcion <- getLine
    case opcion of
        "1" -> mostrarInformacion hotel
        "2" -> cargarTiposHabitacion hotel
        "3" -> asignarCantidadHabitacion hotel
        "4" -> cargarTarifas hotel
        "5" -> consultarReservaciones hotel
        "6" -> estadisticasOcupacion hotel
        "7" -> menuPrincipal hotel
        _   -> do
            putStrLn "Opción inválida"
            menuAdministrativo hotel

-- Función para mostrar el menú de opciones generales
menuGeneral :: Hotel -> IO ()
menuGeneral hotel = do
    putStrLn "Seleccione una opción general:"
    putStrLn "1. Reservación"
    putStrLn "2. Facturar reservación"
    putStrLn "3. Regresar al menú principal"
    opcion <- getLine
    case opcion of
        "1" -> hacerReservacion hotel
        "2" -> facturarReservacion hotel
        "3" -> menuPrincipal hotel
        _   -> do
            putStrLn "Opción inválida"
            menuGeneral hotel

-- Función para mostrar la información del hotel
mostrarInformacion :: Hotel -> IO ()
mostrarInformacion hotel = do
    putStrLn "Información del hotel:"
    putStrLn $ "Nombre: " ++ nombreHotel hotel
    putStrLn "Tipos de habitaciones:"
    mapM_ print (habitaciones hotel)
    menuAdministrativo hotel

-- Función para cargar los tipos de habitaciones
cargarTiposHabitacion :: Hotel -> IO ()
cargar
