--Estructura para las facturas

module Factura where

data Factura=Factura{
    idFactura::String,
    idReserva::String,
    infoLocal::String,
    subTotal::Double,
    total::Double,
    impuesto::Double,
    detalleReserva::String
}