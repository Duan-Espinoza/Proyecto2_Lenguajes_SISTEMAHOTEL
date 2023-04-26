--estructura detalle de reserva usada para agrupar los detalles de las habitaciones y huepedes de cada habitacion en cada reserva

module DetalleReserva where

data DetalleReserva=DetalleReserva{
    idDetalle::String,
    habitacion::String   --esto es el id de la habitacion
    numNinos::Int,
    numAdutos::Int
}deriving(Show,Eq)