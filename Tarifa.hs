--Estructura para las tarifas

module Tarifa where

data Tarifa=Tarifa{
    idTarifa::String,
    mes::String,
    dia::String,
    huesped::String,
    costo::Double
}deriving(Show,Eq)