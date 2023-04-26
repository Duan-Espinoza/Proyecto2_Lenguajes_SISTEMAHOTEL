-- librerias necesarias

import System.IO
import Data.Time

--estructuras del programa

import Habitacion
import Reservacion
import Hotel
import TipoHabitacion
import DetalleReserva
import Factura
import Tarifa


-- Función para mostrar el menú principal
menuPrincipal :: Hotel -> IO ()
menuPrincipal hotel = do
    hotel <- cargarInformacion
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
        "3" -> cantTipoRooms hotel
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
    putStrLn $ "Cédula jurídica: " ++ cedulaJuridica hotel
    putStrLn $ "Sitio web: " ++ sitioWeb hotel
    putStrLn $ "Teléfono: " ++ telefono hotel
    putStrLn $ "País: " ++ pais hotel
    putStrLn $ "Provincia: " ++ provincia hotel
    putStrLn "Tipos de habitaciones:"
    mapM_ print (habitaciones hotel)
    menuAdministrativo hotel

cargarInformacion :: IO Hotel
cargarInformacion = do
    contenido <- readFile "informacion_hotel.txt"
    let [nombre, cedulaJuridica, sitioWeb, telefono, pais, provincia] = lines contenido
    return $ Hotel nombre [] [] cedulaJuridica sitioWeb telefono pais provincia





-- Función para cargar los tipos de habitaciones
cargarTiposHabitacion :: Hotel -> IO ()
cargarTiposHabitacion hotel = do
    putStrLn "Funcionalidad de carga de tipos de habitaciones"
    putStrLn "Ingrese la ruta del archivo de tipos de habitaciones:"
    rutaArchivo <- getLine
    contenido <- readFile rutaArchivo
    let lineas = lines contenido
    let nuevasHabitaciones = map parseHabitacion lineas
    let habitacionesUnicas = eliminarDuplicados (nuevasHabitaciones ++ habitaciones hotel)
    let nuevoHotel = hotel { habitaciones = habitacionesUnicas }
    putStrLn "Tipos de habitaciones cargados exitosamente"
    menuAdministrativo nuevoHotel

-- procesa una línea del archivo y devuelve una nueva habitación con los datos correspondientes. 
parseHabitacion :: String -> Habitacion
parseHabitacion linea = do
    let [tipo, descripcion, maxHuespedesStr] = words linea
    let maxHuespedes = read maxHuespedesStr :: Int
    Habitacion tipo descripcion maxHuespedes


-- eliminar las habitaciones duplicadas en caso de que se hayan cargado tipos de habitaciones que ya existían en el hotel
eliminarDuplicados :: Eq a => [a] -> [a]
eliminarDuplicados [] = []
eliminarDuplicados (x:xs) = x : eliminarDuplicados (filter (/= x) xs)

cantTipoRooms :: Hotel -> IO ()
cantTipoRooms hotel = putStrLn "Funcionalidad no implementada" >> menuAdministrativo hotel

consultarReservaciones :: Hotel -> IO ()
consultarReservaciones hotel = putStrLn "Funcionalidad no implementada" >> menuAdministrativo hotel

estadisticasOcupacion :: Hotel -> IO ()
estadisticasOcupacion hotel = putStrLn "Funcionalidad no implementada" >> menuAdministrativo hotel

hacerReservacion :: Hotel -> IO ()
hacerReservacion hotel = putStrLn "Funcionalidad no implementada" >> menuGeneral hotel

main :: IO ()
main = do
    let hotel = Hotel "Mi Hotel" [] [] "" "" "" "" ""
    menuPrincipal hotel
