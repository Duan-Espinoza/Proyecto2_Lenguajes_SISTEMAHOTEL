-- Definición del tipo de datos para las habitaciones

module Habitacion where

data Habitacion = Habitacion {
  idHabitacion::String,
  tipo :: String,
  descripcion :: String,
  maxHuespedes :: Int
} deriving (Show, Eq)