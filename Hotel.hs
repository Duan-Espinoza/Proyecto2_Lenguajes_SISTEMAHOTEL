-- Definición del tipo de datos para el hotel

import Data.Time

module Hotel where

data Hotel = Hotel {
    nombreHotel :: String,
    habitaciones :: [Habitacion],
    reservas :: [Reserva],
    cedulaJuridica :: String,
    sitioWeb :: String,
    telefono :: String,
    pais :: String,
    provincia :: String
} deriving (Show)