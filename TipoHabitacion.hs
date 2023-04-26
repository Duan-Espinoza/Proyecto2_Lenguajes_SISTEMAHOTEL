--Tipo de habitacion
--estructura usada para guardar los tipos de habitaciones que se cargan desde un archivo txt
--el objetivo de esta estructura es modularizar le programa tanto como sea posible

module TipoHabitacion where

data TipoHabitacion=TipoHabitacion{
	nombre::String,      --esto es el identificador unico
	descripcion::String,
	maximoHuespedes::Int,
	cantidadPorTipo::Int
}deriving(Show,Eq)