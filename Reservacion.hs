import DetalleReserva

--Definicion de la estructura reservacion 

module Reservacion where

data Reservacion=Reservacion{
    idReservacion::String,
    persona::String,     
    fechaHoraReserva::UTCTime,
    fechaIngreso::UTCTime,
    fechaSalida::UTCTime,
    numNinos::Int,
    numAdultos::Int,
    estado::String,
    total::Double,
    detalleHabitaciones::[String]  --es una lista con los id de cada Detalle para mantener todo lo mas normalizado posible
}deriving(Show,Eq)