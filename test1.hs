    -- Area de pruebas

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
        putStrLn "4. Consultar reservaciones"
        putStrLn "5. Estadísticas de ocupación"
        putStrLn "6. Regresar al menú principal"
        opcion <- getLine
        case opcion of
            "1" -> mostrarInformacion hotel
            "2" -> cargarTiposHabitacion hotel
            "3" -> putStrLn "Funcionalidad no implementada" >> menuAdministrativo hotel
            "4" -> consultarReservaciones hotel
            "5" -> estadisticasOcupacion hotel
            "6" -> menuPrincipal hotel
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
            "2" -> putStrLn "Funcionalidad no implementada" >> menuGeneral hotel
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
    cargarTiposHabitacion hotel = do
        putStrLn "Funcionalidad de carga de tipos de habitaciones"
        -- Aquí puedes implementar la funcionalidad para cargar los tipos de habitaciones
        menuAdministrativo hotel

    consultarReservaciones :: Hotel -> IO ()
    consultarReservaciones hotel = putStrLn "Funcionalidad no implementada" >> menuAdministrativo hotel

    estadisticasOcupacion :: Hotel -> IO ()
    estadisticasOcupacion hotel = putStrLn "Funcionalidad no implementada" >> menuAdministrativo hotel

    hacerReservacion :: Hotel -> IO ()
    hacerReservacion hotel = putStrLn "Funcionalidad no implementada" >> menuGeneral hotel

    main :: IO ()
    main = do
        let hotel = Hotel "Mi Hotel" [] []
        menuPrincipal hotel


--Nota: Cuidado con la tabulación, tira errores parse en caso de hacerlo mal