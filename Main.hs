{-/
System.IO: Este módulo proporciona funciones para la entrada y salida de archivos y dispositivos.

Control.DeepSeq: Se utiliza para forzar la evaluación completa de una estructura de datos. 

Control.Exception: Este módulo proporciona funciones para manejar excepciones en Haskell. Incluye funciones para lanzar y capturar excepciones.

Data.Time.Clock: Este módulo proporciona tipos y funciones para trabajar con fechas y horas en tiempo universal (UTC).

Data.Time.Calendar: Este módulo proporciona tipos y funciones para trabajar con fechas en el calendario gregoriano.

Data.Time.LocalTime: Este módulo proporciona tipos y funciones para trabajar con fechas y horas en una zona horaria local.

Data.Array: Este módulo proporciona funciones para trabajar con matrices (arrays).

System.Console.Haskeline: Este módulo proporciona funciones para leer la entrada del usuario a través de la línea de comandos y realizar algunas validaciones.

-}

import qualified System.IO as IO
import System.IO
import Control.DeepSeq
import Data.Time.LocalTime
import Control.Exception
import Control.Exception (evaluate)
import System.Console.Haskeline (Interrupt(Interrupt))
import Data.Time.Clock 
import Data.Time.Calendar 
import Data.Array (listArray)

type FilePath = String  --Define el string como un tipo de dato que indica la ruta de un archivo

{--------------------------------------------------------------------------------------------------------------
--------------------------------------------Manejo de Documentos-----------------------------------------------
---------------------------------------------------------------------------------------------------------------}

--Entrada: Una dirección del archivo a editar y la cadena de texto que se va a agregar
--Salida: Agrega una linea al documento con la información suministrada como argumento de la funcion
--Restricciones: La ruta del archivo debe ser valida 
--Objetivo: Agrega una linea a un documento indicado

addLineaDocu :: (System.IO.FilePath, [Char]) -> IO ()
addLineaDocu (rutaDocu, contenidoDocument) = do
    appendFile rutaDocu (contenidoDocument ++ "\n")

--Entrada: Una lista de cadenas de caracteres
--Salida: Una cadena de caracteres con la informacion contenida en la lista
--Restricciones: el parametro debe ser una lista de cadenas de caracteres
--Objetivo: Crear una cadena de caracteres con toda la información que se extrae de un documento txt

crearStringDeDocumento::[String]->IO String
crearStringDeDocumento[]=return""
crearStringDeDocumento lista=do
    resto <- crearStringDeDocumento(tail lista)
    return $ show(head lista) ++ "\n" ++ resto

--Entrada: La ruta y nombre del documento txt
--Salida: Escribe en el documento indicado la informacion del place holder cadenaTexto
--Restricciones: Debe ser una ruta valida y el documento debe ser txt
--Objetivo: Escribe en el documento archivo lo que el placeHolder cadenaTexto contiene, si tiene otro contenido lo cambia por el nuevo

editarDocumento :: System.IO.FilePath -> String -> IO ()
editarDocumento documento cadenaTexto = do
    writeFile documento (cadenaTexto)

--Entrada: La ruta de un archivo
--Salida:El contenido de un archivo dentro de una lista, cada renglon del archivo es un campo de la lista
--Restricciones: Que la ruta sea un txt
--Objetivo: Lee un archivo txt

leerDocumento :: IO.FilePath -> IO [String]
leerDocumento rutaArchivo = do
    contenidoArchivo <- IO.readFile rutaArchivo               --readFile es una funcion de la libreria IO que retorna el contenido del archivo como cadena de caracteres
    evaluate (Control.DeepSeq.force contenidoArchivo)         --evaluate se asegura que se sea todo el archivo en memoria correctamente y evitar la caracteristica de evaluacion perezosa de haskell
    let renglones =lines contenidoArchivo                     --lines es la función de parseo de la libreria de haskell
    return renglones;

--Entrada: La ruta del archivo a modificar y el string con los datos
--Salida: Escribe en el archivo indicado los datos
--Restricciones: Debe indicarse una ruta valida
--Objetivo: Modifica en el archivo de la ruta el contenido con los datos del string

documentoHabitacionesModificado :: System.IO.FilePath->[String]->IO ()
documentoHabitacionesModificado documento [] = putStrLn "¡Cargados!\n"
documentoHabitacionesModificado documento lista = do
    let inicio = head lista
    let resto = tail lista
    appendFile documento (inicio ++ "\n")
    documentoHabitacionesModificado documento resto

{--------------------------------------------------------------------------------------------------------------
----------------------------------------------Manejo de Listas-------------------------------------------------
---------------------------------------------------------------------------------------------------------------}

--Objetivo: Determinar si una lista contiene el string indicado
--Entrada: un String y una lista de cadenas de caracteres
--Salida: true si la lista contiene el string, false en caso contrario
--Restricciones: Los argumentos deben ser una lista de strings y un string

contenidoEnLista :: String->[String]->Bool
contenidoEnLista cadenaTexto [] = False
contenidoEnLista cadenaTexto lista = do
    let inicio = head lista
    let resto = tail lista
    if inicio == cadenaTexto then
        True
    else
        contenidoEnLista cadenaTexto resto

{--------------------------------------------------------------------------------------------------------------
---------------------------------------------Conversion de tipos----------------------------------------------
---------------------------------------------------------------------------------------------------------------}
--Entrada: Una cadena de texto
--Salida: Un valor entero que refleja la cadena recibida
--Restricciones:
--Objetivo: Pasar valores de cadena de caracteres a enteros equivalentes

stringInt string = do
    let entero = read string :: Integer
    return entero

--Entrada: Un número entero
--Salida: Un string que refleja en numero recibido
--Restricciones: 
--Objetivo: Convierte un numero entero a su representacion en cadena de texto

intString  num = do
    let cadenaString = show num
    return cadenaString


{--------------------------------------------------------------------------------------------------------------
--------------------------------------Captura y manejo de fecha y hora-----------------------------------------
---------------------------------------------------------------------------------------------------------------}
--Entrada: 
--Salida: Un string con la fecha y hora del sistema
--Restricciones: 
--Objetivo: Obtener la fecha y hora del sistema

fecha :: IO String
fecha = do
    varFecha<-getCurrentTime
    varZona<-getCurrentTimeZone

    let (anio,mes,dia) = toGregorian $ utctDay varFecha --usa la librería Time para obtener el anio, mes y dia
    let (TimeOfDay hora min seg) = localTimeOfDay $ utcToLocalTime varZona varFecha --usa la librería Time para obtener la hora, minuto y segundo
    
    --convierte los valores del tiempo 

    currentHour<-intString hora
    currentMinute<-intString min
    currentSecond<-intString seg

    --convierte los valores de la fecha

    currentYear<-intString anio
    currentMonth<-intString mes
    currentDay<-intString dia
    
    
    let stringFecha = currentYear++"-"++currentMonth++"-"++currentDay++"|"++currentHour++":"++currentMinute++":"++currentSecond
    return stringFecha

--Objetivo: muestra un error en pantalla y vuelve a las reservaciones 
--Salida: --
--Restricciones: --

fechaInvalida ::IO ()
fechaInvalida = do
        putStrLn "La fecha fue incorrecta"
        ralizarReserva
        

{--------------------------------------------------------------------------------------------------------------
--------------------------------------Funciones secundarias necesarias-----------------------------------------
---------------------------------------------------------------------------------------------------------------}

--Entrada: --
--Salida: --
--Restricciones:--
--Objetivo: valida la carga de cantidades

verifCargCantidades::IO()
verifCargCantidades = do
    validar <- leerDocumento "verifNumTipos.txt"
    let valido = head validar
    if (valido == "Si") then
        printNoCarga
    else
        listadoHabitaciones

--Entrada: Dos listas de strings 
--Salida: --
--Restricciones: --
--Objetivo: imprime la cantidad de habitaciones

imprimeInfoHabitacionesAuxiliar::[String]->[String]->IO()
imprimeInfoHabitacionesAuxiliar [] [] = opcionAdmin
imprimeInfoHabitacionesAuxiliar lista1 lista2 = do
    let nombre = head lista1
    let resto = tail lista1 
    let resto2 = tail resto
    let cantidad = read(head resto)::Int
    let mensaje = "La Habitacion "++nombre++" tiene las habitaciones, con id: \n"
    putStrLn mensaje
    manejoDeAdyacentes resto2 cantidad 0 lista2
   

--Entrada:  Dos lista de String y un entero
--Salida: Una lista de cadenas de caracteres
--Restricciones: no
--Objetivo: descartar lineas no necesarias

saltoDeContinuos::[String]->Int->[String]->[String]
saltoDeContinuos lista 0 resto = resto
saltoDeContinuos lista num resto = do
    let informacion = resto++[head lista]
    saltoDeContinuos (tail lista) (num-1) informacion

identificadorFactura::String->String
identificadorFactura strID = do
    let idFactura = "F00"++strID
    idFactura

--Entrada: --
--Salida: --
--Restricciones:--
--Objetivo: muestra un msj de error de carga

printNoCarga::IO()
printNoCarga = do
    let mensaje = "Las cantidades ya se cargaron\n"
    putStrLn mensaje
    opcionAdmin 

errorEnFactura::String->IO()
errorEnFactura idReserva = do
    putStrLn ("La reserva "++idReserva ++ " no existe.\n")
    facturar
    
--Entrada: un String
--Salida: --
--Restricciones: la ruta del archivo debe ser valida
--Objetivo: cambia el estado de las facturas

changeStateFactura::String->IO()
changeStateFactura idReserva = do
    lista <-leerDocumento "reservaciones.txt"
    editarDocumento "reservaciones.txt" ""
    changeStateFactura2 lista idReserva

--Entrada: Una lista de String y un String
--Salida: --
--Restricciones: Que sea un String y una lista de strings
--Objetivo: Se encarga de cambiar el estado de una reservacion en Facturado

changeStateFactura2 :: [String] -> String -> IO()
changeStateFactura2 [] _ = opcionGeneral
changeStateFactura2 (reserva:restoReservas) idReser
  | identificador == idReser = do
        let reservarSi = identificador ++ "\n" ++ nombreReserva ++ "\n" ++ fechaReserva ++ "\n" ++ fechaIngreso ++ "\n" ++ fechaSalida ++ "\n" ++ cantAdultos ++ "\n" ++ cantNinos ++ "\n" ++ "Facturado" ++ "\n"++total++"\n"
        appendFile "reservaciones.txt" reservarSi
  | otherwise = do
        let reservarNo = identificador ++ "\n" ++ nombreReserva ++ "\n" ++ fechaReserva ++ "\n" ++ fechaIngreso ++ "\n" ++ fechaSalida ++ "\n" ++ cantAdultos ++ "\n" ++ cantNinos ++ "\n" ++ estado ++ "\n"++total++"\n"
        appendFile "reservaciones.txt" reservarNo
  where [identificador, nombreReserva, fechaReserva, fechaIngreso, fechaSalida, cantAdultos, cantNinos, estado, total] = words reserva
        tl9 = restoReservas

{--------------------------------------------------------------------------------------------------------------
---------------------------------------Opciones Menu Administrativo--------------------------------------------
---------------------------------------------------------------------------------------------------------------}

--Entrada: Una ruta donde se ubica el archivo hotel.txt que contiene la información del hotel
--Salida: Imprime la información del hotel
--Restricciones: 
--Objetivo: Imprime los datos del hotel

imprimirInfoHotel:: IO()
imprimirInfoHotel = do
    lista <- leerDocumento "hotel.txt"
    mensaje <- crearStringDeDocumento lista
    putStrLn "*******************************"
    putStrLn mensaje
    putStrLn "\n"
    opcionAdmin

--Objetivo: escribir los códigos que se generen en el archivo de codigos
--Entrada: dos archivos los cuales son pasados entre funciones, un string para seguir generando codigos de ser necesarios 2 enteros que pararan las recurciones una lista y otro string que será un mensaje
--Salida: --
--Restricciones: que los archivos existan

escribirCantidades::System.IO.FilePath->String->Integer->Integer->[String]->String->System.IO.FilePath->IO()
escribirCantidades archivo nombre cant cont2 lista mensaje archivo2=do 
    appendFile archivo2 mensaje 
    generarHabitaciones archivo nombre cant cont2 lista archivo2

--Objetivo: carga los archivos en 2 listas para enviarlos a la funcion de printearCantidadesAux
--Entrada: --
--Salida: --
--Restricciones: --

imprimirCantHabitacionesInfo::IO()
imprimirCantHabitacionesInfo = do
    lista1 <- leerDocumento"codigosHabitaciones.txt"
    lista2<- leerDocumento"numHabitaciones.txt"
    imprimeInfoHabitacionesAuxiliar lista2 lista1

--Entrada: Una lista de strings y 2 enteros que detendrán las iteraciones
--Salida: --
--Restricciones: la lista cortada
--Objetivo: corta la lista cant de elementos

arrayParserList:: [String]->Int->Int->[String]
arrayParserList [] cant cont = []
arrayParserList lista cant cont = do
    let resto = tail lista
    let cont2 = cont+1
    if cant == cont then
        resto
    else
        arrayParserList resto cant cont2

--Entrada: Una lista de strings y 2 enteros 
--Salida: --
--Restricciones: --
--Objetivo:identifica la cantidad de elementos de la lista

manejoDeAdyacentes::[String]->Int->Int->[String]->IO()
manejoDeAdyacentes lista cantidad contador lista2 = do
    let encabezado = head lista2
    let resto = tail lista2
    let cont2 = contador+1
    putStrLn (encabezado++"\n")
    if contador == cantidad-1 then 
        imprimeInfoHabitacionesAuxiliar lista resto
    else
        manejoDeAdyacentes lista cantidad cont2 resto

--------------------------------------Reservación--------------------------------------------------------------------------------

--Objetivo: Validar una fecha
--Entrada: 6 Strings los cuales son dia mes año de ingreso y salida, los cambia de string a entero y valida que sean coherentes
--Salida: bool
--Restricciones: que las entradas sean 6 strings de numeros

verificarFecha::String->String->String->String->String->String->Bool
verificarFecha dia1 mes1 anio1 dia2 mes2 anio2 = do
  let anioIngreso = read anio1::Int
  let anioSalida = read anio2 ::Int
  let mesIngreso = read mes1 ::Int
  let mesSalida = read mes2 ::Int
  let diaIngreso = read dia1 ::Int
  let diaSalida = read dia2 ::Int
  if anioSalida > anioIngreso then
    True
  else
    if anioSalida == anioIngreso then
      if mesSalida > mesIngreso then
        True 
      else
        if mesSalida == mesIngreso then
          if diaSalida > diaIngreso then
            True 
          else
            False
        else
          False
    else
      False

--Objetivo: Obtener todos los datos necesarios para una reservacion
--Entrada: --
--Salida: --
--Restricciones: --

ralizarReserva::IO()
ralizarReserva = do
    -- fecha de ingreso
    putStrLn "Indique el dia de ingreso"
    pDia <- getLine
    putStrLn "Indique el mes de ingreso"
    pMes <- getLine
    putStrLn "Indique el anio de ingreso"
    pAnio <- getLine
    let fechaIngreso = [pDia]++[pMes]++[pAnio]
    let fechaI = pDia++"/"++pMes++"/"++pAnio

    -- fecha de salida
    putStrLn "Indique el dia de salida"
    pDiaS <- getLine
    putStrLn "Indique el mes de salida"
    pMesS <- getLine
    putStrLn "Indique el anio de salida"
    pAnioS <- getLine
    let fechaSalida = [pDiaS]++[pMesS]++[pAnioS]
    let fechaS = pDiaS++"/"++pMesS++"/"++pAnioS
    -- datos extra
    putStrLn "Indique cantidad de adultos"
    pAdultos <- getLine
    putStrLn "Indique la cantidad de niños"
    pNinos <- getLine
    putStrLn "Indique el nombre de la persona que reserva"
    pNombre <- getLine
    let res = [fechaI]++[fechaS]++[pAdultos]++[pNinos]++[pNombre]
    if verificarFecha pDia pMes pAnio pDiaS pMesS pAnioS then
        reservarHabitacion res fechaIngreso fechaSalida
    else    
        fechaInvalida 
    
--Objetivo: Crea 1 lista con las cantidades de huespedes maximos por habitacion y los tipos de habitaciones
-- otra lista con las reservaciones por fecha para validar la dispomnibilidad de los tipos de habitaciones
-- y otra con las habitaciones y la cantidad de habitaciones por tipo en el hotel para pasarlas a las siguientes funciones
--Entrada: 3 listas de strings la lista que tendra todos los resultados de los datos que se reservarán, listas de fechas
--Salida: --
--Restricciones: que los 3 parametros sean 3 listas de strings

reservarHabitacion::[String]->[String]->[String]->IO()
reservarHabitacion res fIngreso fSalida = do
    lista0 <- leerDocumento"habitaciones.txt"
    listaHabCanHab <- leerDocumento"numHabitaciones.txt" --lista de cantidad de habitaciones en hotel
    listareservasporfecha <- leerDocumento"reservacionesporFecha.txt" --lista de las reservaciones por fechas
    let listaHabHues = listadoHabitacionesAux2 lista0 [] -- Lista de habitaciones y cantidad maxima de huspedes
    reservaporhabitacionAux listaHabHues res listaHabCanHab 0 fIngreso fSalida listareservasporfecha []

--Objetivo: Obtener los datos de las cantidades de habitaciones a reservar por sus tipos, asi como las cantidades de adultos y niños, la funcion verifica la disponibilidad de las habitaciones.
--Entrada: 3 listas de strings, resultados, fechas, 1 entero que es la cantidad de huespedes total, debera coincidir con los huespedes
-- indicados al inicio, las listas creadas en la anterior funcion y una lista que contendrá las habitaciones resevadas, las cantidades y las cantidades de niños y adultos.
--Salida: --
--Restricciones: que las entradas sean 7 listas de strings y 1 entero

reservaporhabitacionAux::[String]->[String]->[String]->Int->[String]->[String]->[String]->[String]->IO()
reservaporhabitacionAux [] res listaHabCanHab cantHuesp fIngreso fSalida listareservasporfecha habitacionesReservadas = validarHuespedes cantHuesp res fIngreso fSalida listareservasporfecha habitacionesReservadas--xd res es la suma de huespedes total
reservaporhabitacionAux listaHabHues res listaHabCanHab cantHuesp fIngreso fSalida listareservasporfecha habitacionesReservadas = do
    let nombre = head listaHabHues --nombre de habitacion
    let tl = tail listaHabHues 
    let tlCanHabSinNombre = tail listaHabCanHab --cortamos la lista de las cantidades de habitaciones totales del hotel
    let totalHabs = head tlCanHabSinNombre -- tomamos la cantidad máxima de la habitacion del hotel
    let sigTotHab = tail tlCanHabSinNombre -- Siguientes habitaciones totales
    let totalHabitaciones = read totalHabs::Int --tomamos la cantidad máxima de la habitacion del hotel en Int
    let cantHues = head tl -- cantidad maxima de huespedes de habitacion en string
    let cantidadHuespedes = read cantHues::Int -- cantidad maxima de huespedes de habitacion en int
    let tl1 = tail tl  --lista de habitaciones y cantidades cortada 
    let cantidadHabitacionesReservadasFecha = listaRangoFecha nombre fIngreso fSalida listareservasporfecha --numero de habitaciones reservadas para esta fecha
    let habitacionesDisponibes = totalHabitaciones - cantidadHabitacionesReservadasFecha --habitaciones disponibles para reservar
    putStrLn ("Cantidad de habitaciones " ++ nombre ++ " a reservar: ")
    pCantTipoH <- getLine
    let pCantTipoH2 = read pCantTipoH::Int --conversion de string a int
    putStrLn ("Cantidad de adultos por habitación " ++ nombre ++ ": ")
    pCantAdTipoH <- getLine
    let pCantAdTipoH2 = read pCantAdTipoH::Int --conversion de string a int
    putStrLn ("Cantidad de niños por habitación " ++ nombre ++ ": ")
    pCantNiTipoH <- getLine
    let listaReservaciones = habitacionesReservadas++[nombre]++[pCantTipoH]++[pCantAdTipoH]++[pCantNiTipoH] 
    let pCantNiTipoH2 = read pCantNiTipoH::Int --conversion de string a int
    let pSuma = (pCantAdTipoH2 + pCantNiTipoH2)*pCantTipoH2 --suma de huespedes multiplicado por la cantidad de habitaciones
    let res2 = cantHuesp+pSuma  --Cantidad de huespedes totales de momento
    let res3 = res++[nombre]++[pCantTipoH]++[pCantAdTipoH]++[pCantNiTipoH]  --Resultado con las cantidades de la habitacion habitacion/adultos/niños
    if pSuma <= cantidadHuespedes*pCantTipoH2 then
        if habitacionesDisponibes >= pCantTipoH2 then
            reservaporhabitacionAux tl1 res3 sigTotHab res2 fIngreso fSalida listareservasporfecha listaReservaciones 
        else
            errorHabitaciones listaHabHues res listaHabCanHab cantHuesp fIngreso fSalida listareservasporfecha habitacionesReservadas
    else
        errorSumaCantidad listaHabHues res listaHabCanHab cantHuesp fIngreso fSalida listareservasporfecha habitacionesReservadas
        
--Objetivo: crear el identificador de las reservaciones
--Entrada:  1 String
--Restricciones: que la entrada sea 1 lista de strings

identificadorReserva::String->String
identificadorReserva strID = do
    let idF = "R"++strID
    idF

--Objetivo: verificar que la cantidad de huespedes indicada al inicio, coincida con la cantidad de huespedes final
--Entrada:  5 listas de strings necesarias para seguir con las funciones de reservaciones y 1 entero que es la que hay que validar
--Restricciones: que las entradas sean 1 entero y 5 strings

validarHuespedes::Int->[String]->[String]->[String]->[String]->[String]->IO()
validarHuespedes cantHuesp resultado fIngreso fSalida listareservasporfecha habitacionesReservadas = do
    let listasinFechas = arrayParserList resultado 1 0
    let strCantAd = head listasinFechas
    let intCantAd = read strCantAd::Int
    let tl = tail listasinFechas
    let strCantNi = head tl
    let intCantNi = read strCantNi::Int 
    let sumaCant = intCantAd+intCantNi
    if sumaCant == cantHuesp then
        mostrarReservacionCorrecta resultado fIngreso fSalida listareservasporfecha habitacionesReservadas
    else
        errorCantHuespedes

--Objetivo: mostrar los datos de la reservacion concluida, así como de guardar los datos en los archivos correspondientes
--Entrada:  5 listas de strings necesarias para mostrar los datos de una reservacion correcta.
--Restricciones: que las entradas sean 5 listas de strings

mostrarReservacionCorrecta::[String]->[String]->[String]->[String]->[String]->IO()
mostrarReservacionCorrecta res fIngreso fSalida listareservasporfecha listaReservaciones = do
    lista <- leerDocumento "codigoReservacion.txt"
    lista2 <- leerDocumento "Tarifas.txt"
    let strID = head lista
    let idReserva = identificadorReserva strID
    let intID = read strID::Int
    let intTotal =  calcularTotal listaReservaciones lista2 0
    let strTotal = show intTotal
    let intIDNew = intID + 1
    let strIDNew = show intIDNew
    let listaNombre = arrayParserList res 3 0 
    let nombre = head listaNombre
    let fechaI = head res
    let listasinFI = tail res
    let fechaS = head listasinFI
    let listasinFechas = tail listasinFI
    let cantAd = head listasinFechas
    let listasinFechasCAdultos = tail listasinFechas
    let cantNi = head listasinFechasCAdultos
    fechaActual <- fecha
    editarDocumento "codigoReservacion.txt" strIDNew
    putStrLn "\n ******************** Factura ******************** \n"
    putStrLn ("\nId Reservacion: " ++ idReserva ++ "\n")
    putStrLn ("Nombre de la persona que reserva: " ++ nombre ++ "\n")
    putStrLn ("Fecha en la que se realizo la reservacion: " ++ fechaActual ++" \n")
    putStrLn ("Fecha de ingreso: " ++ fechaI ++ " \n")
    putStrLn ("Fecha de salida: " ++ fechaS ++ " \n")
    putStrLn ("Cantidad de adultos: " ++ cantAd ++ " \n")
    putStrLn ("Cantida de ninos: " ++ cantNi ++ " \n")
    putStrLn ("Total:  " ++ strTotal ++ " \n")
    appendFile "reservaciones.txt" (idReserva++ "\n")
    appendFile "reservaciones.txt" (nombre++ "\n")
    appendFile "reservaciones.txt" (fechaActual++ "\n")
    appendFile "reservaciones.txt" (fechaI++ "\n")
    appendFile "reservaciones.txt" (fechaS++ "\n")
    appendFile "reservaciones.txt" (cantAd++ "\n")
    appendFile "reservaciones.txt" (cantNi ++ "\n")
    appendFile "reservaciones.txt" ("Activa" ++ "\n")
    appendFile "reservaciones.txt" (strTotal ++ "\n")
    mostrarHabitacionesReservadas res fIngreso fSalida listareservasporfecha listaReservaciones idReserva

--Objetivo: calcular los precios de las habitaciones
--Entrada: la lista de las habitaciones del hotel con sus tarifas y la lista de las reservaciones como la variable resultadoque es un entero
--Restricciones: que las entradas sean 2 listas de strings y 1 entero

calcularTotal::[String]->[String]->Int->Int
calcularTotal [] listaTarifas suma = suma
calcularTotal listaReservadas listaTarifas suma = do
    let habitacionReservada = head listaReservadas
    let habitacionTarifa = head listaTarifas
    let cantidadReservadas = read (head(tail listaReservadas))::Int
    let cantidadTarifa = read (head(tail listaTarifas))::Int
    let suma2 = suma+(cantidadReservadas * cantidadTarifa)
    if habitacionReservada == habitacionTarifa then
        calcularTotal (arrayParserList listaReservadas 3 0) (arrayParserList listaTarifas 1 0) suma2
    else
        calcularTotal listaReservadas (arrayParserList listaTarifas 1 0) suma

--Objetivo: verificar que las cantidades de las habitaciones sean mayor a 0 para imprimirlas
--Entrada: 5 listas de strings necesarias para mostrar los datos de una reservacion correcta y 1 string que es el id de la reservacion.
--Restricciones: que las entradas sean 5 listas de strings y 1 string
    

mostrarHabitacionesReservadas::[String]->[String]->[String]->[String]->[String]->String->IO()
mostrarHabitacionesReservadas  res fIngreso fSalida listareservasporfecha [] id = terminarReservacion
mostrarHabitacionesReservadas  res fIngreso fSalida listareservasporfecha listaReservaciones id= do
    let nombre = head listaReservaciones 
    let listaCant = tail listaReservaciones
    let cant = head listaCant --
    let cantINT = read cant::Int --
    let listaAD = tail listaCant
    let cantAd = head listaAD
    let listaNI = tail listaAD
    let cantNi = head listaNI
    let sigLista = arrayParserList listaReservaciones 3 0 
    if cantINT == 0 then
        mostrarHabitacionesReservadas res fIngreso fSalida listareservasporfecha sigLista id
    else
        mostrarHabitacionesReservadasAux res fIngreso fSalida listareservasporfecha sigLista nombre cantINT cantAd cantNi id

--Objetivo: Guarda las reservaciones de las habitaciones por fecha en el archivo correspondiente
--Entrada: 5 listas de strings necesarias para mostrar los datos de una reservacion correcta y 4 strings que es el id de la reservacion los datos a guardar y 1 int.
--Restricciones: que las entradas sean 5 listas de strings, 4 string  y 1 entero

mostrarHabitacionesReservadasAux::[String]->[String]->[String]->[String]->[String]->String->Int->String->String->String->IO()
mostrarHabitacionesReservadasAux res fIngreso fSalida listareservasporfecha listaReservaciones nombre cantidadINT adultos ninos id= do
    let cantidadHabitacionesReservadasFecha = listaRangoFecha nombre fIngreso fSalida listareservasporfecha --numero de habitaciones reservadas para esta fecha
    appendFile "reservacionesporFecha.txt" (head fIngreso ++"\n" ) --dia Ingreso
    appendFile "reservacionesporFecha.txt" (head (tail fIngreso)++"\n") -- mes Ingreso
    appendFile "reservacionesporFecha.txt" (head (tail (tail fIngreso))++"\n") --año Ingreso
    appendFile "reservacionesporFecha.txt" (head fSalida ++"\n")  --dia Salida
    appendFile "reservacionesporFecha.txt" (head (tail fSalida)++"\n") -- mes Salida
    appendFile "reservacionesporFecha.txt" (head (tail (tail fSalida))++"\n") --año Salida
    appendFile "reservacionesporFecha.txt" (nombre++"\n") --nombre
    appendFile "reservacionesporFecha.txt" (show cantidadINT++"\n") --nombre
    mostrarHabitacionesReservadasAux2 res fIngreso fSalida listareservasporfecha listaReservaciones nombre cantidadINT adultos ninos cantidadHabitacionesReservadasFecha id

--Objetivo: muestra las habitaciones reservadas con los identificadores y guarda las habitaciones en el archivo.
--Entrada: 5 listas de strings necesarias para mostrar los datos de una reservacion correcta y 4 strings que es el id de la reservacion los datos a guardar y 2 int.
--Restricciones: que las entradas sean 5 listas de strings, 4 string  y 2 entero

mostrarHabitacionesReservadasAux2::[String]->[String]->[String]->[String]->[String]->String->Int->String->String->Int->String->IO()
mostrarHabitacionesReservadasAux2 res fIngreso fSalida listareservasporfecha listaReservaciones nombre 0 adultos ninos num id = mostrarHabitacionesReservadas res fIngreso fSalida listareservasporfecha listaReservaciones id
mostrarHabitacionesReservadasAux2 res fIngreso fSalida listareservasporfecha listaReservaciones nombre cantidadINT adultos ninos num id = do
    let idHabitacion = nombre++show num
    let habitacion = "Habitacion id: " ++ idHabitacion ++" tipo: "++nombre++" "++"Cantidad de adultos: " ++ adultos ++" "++"Cantidad de ninos: " ++ ninos
    putStrLn habitacion
    appendFile "habitacionesReservadas.txt" (id++"\n")
    appendFile "habitacionesReservadas.txt" (habitacion++ "\n")
    mostrarHabitacionesReservadasAux2 res fIngreso fSalida listareservasporfecha listaReservaciones nombre (cantidadINT-1) adultos ninos (num+1) id
    
--Objetivo: muestra un error de verificacion y envia al usuario a reservaciones.
--Entrada: --
--Restricciones: --

errorCantHuespedes::IO()
errorCantHuespedes = do
    putStrLn "La cantidad de huespedes indicada al inicio no coincide con la cantidad total"
    ralizarReserva

--Objetivo: Mostrar un error y seguir reservando
--Entrada: 7 listas de strings, que guardan los datos de la reservacion hasta el momento
--Restricciones: que hayan 7 listas de strings y 1 entero

errorSumaCantidad::[String]->[String]->[String]->Int->[String]->[String]->[String]->[String]->IO()
errorSumaCantidad  listaHabHues res listaHabCanHab cantHues fIngreso fSalida listareservasporfecha habitacionesReservadas = do
    putStrLn "Error la cantidad de huespedes supera el limite de las habitaciones\n"
    reservaporhabitacionAux listaHabHues res listaHabCanHab cantHues fIngreso fSalida listareservasporfecha habitacionesReservadas

--Objetivo: Sacar la cantidad de habitaciones de un tipo en un determinado rango de fechas
--Entrada: 1 String nombre del tipo, 3 lista de Strings la lista de las reservaciones por fecha, las fechas de ingreso y de salida 
--Restricciones: que las entradas sean String, 3 listas de strings

listaRangoFecha::String->[String]->[String]->[String]->Int
listaRangoFecha pHabitacion fIngreso fSalida listaFechas = do
    let diaI = head fIngreso
    let diaF = head fSalida
    let diaIni = read diaI::Int
    let diaFin = read diaF::Int
    let fIngreso2 = tail fIngreso
    let mesI = head fIngreso2
    let mesIni = read mesI::Int
    listaRangoFechaAux pHabitacion listaFechas 0 diaIni diaFin mesIni 

--Objetivo: Sacar la cantidad de habitaciones de un tipo en un determinado rango de fechas
--Entrada: 1 String nombre del tipo, Una lista de Strings la lista de las reservaciones por fechas
-- 4 enteros, donde estará el resultado y se suman las habitaciones que coincidan con el rango, el dia de ingreso y de salida y mes  
--Salida: un numero con la cantidad de habitaciones reservadas en una fecha
--Restricciones: que las entradas sean String, listas de strings y 4 enteros

listaRangoFechaAux::String->[String]->Int->Int->Int->Int->Int
listaRangoFechaAux pHabitacion [] res diaIni diaFin mesIni= res
listaRangoFechaAux pHabitacion lista res diaIni diaFin mesIni  = do
    let hd = head lista
    let dia1 = read hd::Int
    let tl = tail lista
    let hd2 = head tl
    let mes1 = read hd2::Int 
    let habitaciones = arrayParserList lista 5 0
    let sig = arrayParserList lista 7 0
    let habitacion = head habitaciones
    let cant0 = tail habitaciones
    let cant1 = head cant0
    let cant2 = read cant1::Int
    let res2 = res+cant2
    if dia1 >= diaIni && dia1 <= diaFin && pHabitacion == habitacion && mes1 == mesIni then
        listaRangoFechaAux pHabitacion sig res2 diaIni diaFin mesIni 
    else
         listaRangoFechaAux pHabitacion sig res diaIni diaFin mesIni 

--Objetivo: sacar cada 1 elemento de una lista en este caso los nombres y cantidades, solo crea una lista de nombres de habitaciones
--Entrada: 2 listas una será el resultado y la otra es la lista de tipos de habitaciones con nombre, descripcion y cantidades
--Salida: una lista con solo los nombres y cantidades maximas de huespedes de las habitaciones
--Restricciones: que las entradas sean listas de strings

listadoHabitacionesAux2:: [String]->[String]->[String]
listadoHabitacionesAux2 [] res = res
listadoHabitacionesAux2 lista res = do
    let hd = head lista
    let tl = tail lista
    let tl1 = tail tl
    let tl2 = tail tl1
    let res2 = res++[hd]++[head tl1]
    listadoHabitacionesAux2 tl2 res2

--Objetivo: muestra un error en pantalla y vuelve a las reservaciones por habitación
--Salida: --
--Restricciones: que las entradas sean 7 listas de strings y 1 entero

errorHabitaciones::[String]->[String]->[String]->Int->[String]->[String]->[String]->[String]->IO()
errorHabitaciones  listaHabHues res listaHabCanHab cantHues fIngreso fSalida listareservasporfecha habitacionesReservadas = do
    putStrLn "Cantidad de habitaciones insuficiente para esta fecha.\n"
    reservaporhabitacionAux listaHabHues res listaHabCanHab cantHues fIngreso fSalida listareservasporfecha habitacionesReservadas

--Objetivo: muestra en pantalla un mensaje de exitoso y vuelve al menu de opciones de usuario 
--Salida: --
--Restricciones: --

terminarReservacion:: IO()
terminarReservacion = do
    putStrLn "\n  +++++++++++++++++++++++++++++++++++\n++++++++++Reservacion Exitosa!!++++++++\n  +++++++++++++++++++++++++++++++++++\n"
    opcionGeneral

----------------------------------------------------Habitaciones---------------------------------------------------------
--Entrada: dos listas de Strings y un entero para recursión
--Salida: La ista de las habitaciones
--Restricciones: debe recibir dos listas
--Objetivo: Enlista cada tres lineas del archivo leído

cargaListasHabitaciones :: [String] -> [String] -> Int -> [String]
cargaListasHabitaciones [] respuesta opcion = respuesta
cargaListasHabitaciones lista respuesta opcion =
    let 
        -- Separamos la cabeza de la cola de la lista original.
        hd = head lista
        tl = tail lista
        -- Creamos una nueva lista resultante que incluye la cabeza de la lista original.
        segundaRespuesta = respuesta ++ [hd]
        opcn1 = opcion + 1
        opcn2 = opcion + 3
    in
        if (mod opcion 3 == 0) && contenidoEnLista hd respuesta
            -- Si el elemento de la cabeza está repetido en la lista resultante, recursamos con la cola de la cola de la lista original y la misma lista resultante, pero usando la opción 2.
            then cargaListasHabitaciones (tail (tail tl)) respuesta opcn2
            -- Si el elemento de la cabeza no está repetido en la lista resultante, recursamos con la cola de la lista original y la nueva lista resultante, pero usando la opción 1.
            else cargaListasHabitaciones tl segundaRespuesta opcn1

--Entrada: la ruta del archivo con los datos de las habitaciones
--Salida: carga los datos de las habitaciones
--Restricciones: la ruta debe ser valida
--Objetivo: cargar los datos de las habitaciones

leerLosDatosHabitaciones:: IO()
leerLosDatosHabitaciones = do

    --carga los datos del archivo de habitaciones

    listaData <- leerDocumento"habitaciones.txt"
    putStrLn "\nIngrese la direccion del documento con los datos de las habitaciones: "

    --pide la ruta al usuario

    direccion <- getLine
    listaDatosHabitaciones <- leerDocumento direccion

    -- cargamos los datos en una nueva lista con la informacion del documento de habitaciones y el documento indicado en la ruta ingresada

    let listaNueva = cargaListasHabitaciones listaDatosHabitaciones listaData 0
    editarDocumento "habitaciones.txt" ""
    documentoHabitacionesModificado "habitaciones.txt" listaNueva 
    opcionAdmin

--Entrada: la ruta al archivo en el que se indican la cantidad de habitaciones
--Salida: modifica el contenido del archivo "Habitaciones"
--Restricciones: la ruta del archivo debe ser valida
--Objetivo: carga la cantidad de habitaciones

listadoHabitaciones:: IO()
listadoHabitaciones = do

    -- abre la ruta del archuvo y lee el contenido

    contenidoDoc <- leerDocumento"habitaciones.txt"

    --extrae la lista de habitaciones del contenido del archivo

    let listaDatos = listadoHabitaciones2 contenidoDoc []
    editarDocumento "numHabitaciones.txt" ""
    editarDocumento "idHabitaciones.txt" ""
    cargarNumeroHabitaciones "numHabitaciones.txt" listaDatos
    
--Entrada: Lista nueva a retornar y la lista con los datos de las habitaciones
--Salida: Una lista con los datos de las habitaciones
--Restricciones:
--Objetivo: extrae los datos de inicio del archivo de habitaciones, corresponde al nombre

listadoHabitaciones2:: [String]->[String]->[String]
listadoHabitaciones2 [] respuesta = respuesta
listadoHabitaciones2 lista respuesta = do
    let nombresHabitaciones = head lista
    let resto = tail lista
    let descripcionHabitaciones = tail resto
    let numPer = tail descripcionHabitaciones
    let segundaRes = respuesta++[nombresHabitaciones] 
    listadoHabitaciones2 numPer segundaRes

--Objetivo: genera las cantidades para cada tipo de habitaciones y su codigo respectivo
--Entrada: la ruta del archivo con los tipos
--Salida: genera las cantidades de cada tipo de habitaciones
--Restricciones: la ruta del archivo debe ser valida

cargarNumeroHabitaciones:: System.IO.FilePath->[String]->IO()
cargarNumeroHabitaciones archivo [] = imprimirCantHabitacionesInfo
cargarNumeroHabitaciones archivo lista = do
    let nombre = head lista
    let resto = tail lista
    let mensaje = "\tDigite la cantidad de habitaciones " ++ nombre ++ ": "
    editarDocumento "verifNumTipos.txt" ""
    putStrLn mensaje
    cant <- getLine
    appendFile "verifNumTipos.txt" ("Si")
    appendFile archivo (nombre ++ "\n")
    appendFile archivo (cant ++ "\n")
    cant2 <- stringInt(cant)
    generarHabitaciones archivo nombre cant2 0 resto "codigosHabitaciones.txt"

--Objetivo: genera cant cantidades de códigos para una habitación y los envía a escribir al archivo de códigos
--Entrada: un archivo y una lista 2 enteros y otro archivo
--Salida: --
--Restricciones: que los archivos existan

generarHabitaciones:: System.IO.FilePath->String->Integer->Integer->[String]->System.IO.FilePath->IO()
generarHabitaciones archivo nombre cant cont lista archivo2 =do 
    let cont2 = cont + 1
    pCont2 <- intString cont
    let mensaje = nombre++pCont2++"\n"
    if cont < cant then
        escribirCantidades archivo nombre cant cont2 lista mensaje archivo2
    else
        cargarNumeroHabitaciones archivo lista
    
--------------------------Cargar los precios-------------------------
--Entrada: la ruta del txt
--Salida: carga las tarifas
--Restricciones: la ruta debe existir
--Objetivo: carga las tarifas desde el documento

leerDocTarifas::IO()
leerDocTarifas = do
    editarDocumento "Tarifas.txt" ""
    contenidoDocuT <- leerDocumento"habitaciones.txt"
    let lista = listadoHabitaciones2 contenidoDocuT []
    definirMontoTarifas lista

--Entrada: un flotante para definir os montos
--Salida: modifica el archivo de tarifas con los montos
--Restricciones: el usuario debe ingresar un valor numerico
--Objetivo: cargas los montos de las tarifas

definirMontoTarifas ::[String]->IO()
definirMontoTarifas [] = opcionAdmin 
definirMontoTarifas lista = do
    
    --obtiene e nombre de la habitacion

    let name = head lista
    let resto = tail lista

    --imprime el nombre de la recamara

    let msj = "Indique el monto de la habitacion " ++ name ++ " : \n"
    putStrLn msj

    --pide ingresar el precio

    monto <- getLine
    appendFile "Tarifas.txt" (name ++ "\n" ++ monto ++ "\n")
    definirMontoTarifas resto

-----------------------------------Historial de reservaciones--------------------------------

--Objetivo: Se encarga de leer el archivo de reservaciones y hacerla una lista
--Entrada: --
--Salida: --
--Restricciones: --

leerReservHistorial:: IO()
leerReservHistorial = do
    listaReservacion <- leerDocumento "reservaciones.txt"
    leerReservHistorial2 listaReservacion

--Objetivo: Muestra el detalle de la reservación
--Entrada: Una lista de cadenas de texto con la información de la reservación
--Salida: --
--Restricciones: --

leerReservHistorial2 :: [String] -> IO ()
leerReservHistorial2 [] = opcionAdmin
leerReservHistorial2 (idReserva : nombre : fechaRes : fechaIng : fechaSal : cantAd : cantNin : estado : total : rest) = do
    let mensaje = unlines
            [ "Identificador: " ++ idReserva
            , "Nombre de la persona que reservo: " ++ nombre
            , "Fecha de reserva: " ++ fechaRes
            , "Fecha de ingreso: " ++ fechaIng
            , "Fecha de salida: " ++ fechaSal
            , "Cantidad de adultos: " ++ cantAd
            , "Cantidad de niños: " ++ cantNin
            , "Estado: " ++ estado
            , total
            ]
    putStrLn mensaje
    listaHabitacion <- leerDocumento "habitacionesReservadas.txt"
    leerReservHistorial3 listaHabitacion idReserva
    leerReservHistorial2 rest

-- Entrada: Una lista de String y un String identificador de reserva
-- Salida: --
-- Restricciones: que sea un string y una lista de strings
-- Objetivo: Se encarga de validar el detalle de las habitaciones reservadas con el identificador de reserva

leerReservHistorial3 :: [String] -> String -> IO ()
leerReservHistorial3 [] identificador = putStrLn "************************************"
leerReservHistorial3 (idReservacion : info : resto) identificador
  | identificador == idReservacion = imprimirDetallesReservas info resto identificador
  | otherwise = leerReservHistorial3 resto identificador

--Entrada: varios strings y listas
--Salida: imprime los detalles
--Restricciones: que sean dos strings y una lista de strings
--Objetivo: muestra el detalle de las reservaciones

imprimirDetallesReservas::String->[String]->String->IO()
imprimirDetallesReservas msj listInfo ident= do
    putStrLn msj
    leerReservHistorial3 listInfo ident

--Objetivo: Se encarga de leer el archivo de reservaciones y hacerla una lista
--Entrada: --
--Salida: --
--Restricciones: --
mostrarTotalHuespedes:: IO()
mostrarTotalHuespedes = do
    listaReservacion <- leerDocumento "reservaciones.txt"
    let total = verTotalHuespedes listaReservacion 0
    let mensaje = "Numero de huespedes: "++show total++"\n"
    putStrLn mensaje
    estadistic

-- Obj: Mostrar el total de huéspedes con reservas activas o facturadas
-- Entrada: Lista de Strings, entero (acumulador)
-- Salida: Entero

verTotalHuespedes :: [String] -> Int -> Int
verTotalHuespedes [] res = res
verTotalHuespedes (identificador : nombreReserva : fechaReserva : fechaIngreso : fechaSalida : cantAdultos : cantNinos : estado : resto) res
    | estado == "Activa" || estado == "Facturado" = verTotalHuespedes resto (res + suma)
    | otherwise = verTotalHuespedes resto res
    where
        intCantAdultos = read cantAdultos :: Int
        intCantNinos = read cantNinos :: Int
        suma = intCantAdultos + intCantNinos

------------------------------ Facturar ---------------------------------------------
 
--Entrada: el id de la factura
--Salida: ninguna
--Restricciones: debe ser un id valido
--Objetivo: inicia el proceso de facturacion

facturar::IO()
facturar = do
    putStrLn "Ingrese el id de la reserva: "
    idReserva <- getLine
    lista <- leerDocumento "reservaciones.txt"
    let resp = isActive idReserva lista
    if resp then
        facturar2 idReserva
    else
        errorEnFactura idReserva

--Entrada: Un String
--Salida:
--Restricciones: 
--Objetivo: muestra y verifica los codigos de reservacion

facturar2::String->IO()
facturar2 idReserva = do
    lista <- leerDocumento "codigoFactura.txt"
    let numStr = head lista
    let numInt = (read numStr::Int) +1 
    editarDocumento "codigoFactura.txt" (show numInt)
    let idFactura = identificadorFactura numStr
    let res = [idFactura]++[idReserva]
    putStrLn ("\nFactura #" ++ idFactura )
    putStrLn ("Codigo de reservacion: " ++ idReserva ++ " \n")
    facturar3 res

facturar3 :: [String] -> IO ()
facturar3 resFactura = do
    listaReservas <- leerDocumento "reservaciones.txt"
    let reserva = detalleReservacion (head (tail resFactura)) listaReservas
        subtotal = totalReservacion (head (tail resFactura)) listaReservas
        iva = div (read subtotal :: Int) 100 * 13
        total = read subtotal + iva

    imprimirInfoHotel
    putStrLn $ "Subtotal: " ++ subtotal
    putStrLn $ "Impuestos: " ++ show iva
    putStrLn $ "Total: " ++ show total

    appendFile "facturas.txt" $ unlines [head resFactura, head $ tail resFactura, subtotal, show iva, show total]

    putStrLn "\n*************-Detalles***************\n"
    let [identificador, nombreReserva, fechaReserva, fechaIngreso, fechaSalida, cantAdultos, cantNinos, estado, total'] = take 9 listaReservas
        mensaje = unlines ["Id: " ++ identificador,
                           "Nombre del reservante: " ++ nombreReserva,
                           "Fecha de reserva: " ++ fechaReserva,
                           "Fecha de ingreso: " ++ fechaIngreso,
                           "Fecha de salida: " ++ fechaSalida,
                           "Cantidad de adultos: " ++ cantAdultos,
                           "Cantidad de ninos: " ++ cantNinos,
                           "Estado: Facturado",
                           "Total: " ++ total']
    putStrLn mensaje
    putStrLn "\n¡Facturado!\n"
    changeStateFactura (head $ tail resFactura)

-------------------Calcular las ganancias ---------------------------

--Objetivo: Se encarga de leer el archivo de facturas y hacerla una lista ademas de mostrar el total recaudado con impuestos
--Entrada: --
--Salida: --
--Restricciones: --
calculoDeRecaudo:: IO()
calculoDeRecaudo = do
    listaFacturas <- leerDocumento "facturas.txt"
    let monto = calculoDeRecaudo2 listaFacturas 0
    let msj = "Monto recaudado: "++show monto++"\n"
    putStrLn msj
    estadistic

-- Entrada: Una lista de String y un entero
-- Salida: Un entero
-- Restricciones: La entrada debe ser una lista de String y un entero
-- Objetivo: Se encarga de recorrer la lista de facturas y sumar los totales para retornarlo

calculoDeRecaudo2 :: [String] -> Int -> Int 
calculoDeRecaudo2 [] res = res 
calculoDeRecaudo2 lista res = do
    let [_, _, _, _, _, _, _, _, total] = lista
    let totalInt = read total :: Int
    calculoDeRecaudo2 (drop 9 lista) (totalInt + res)

-- Entrada: Un String y una lista de String
-- Salida: Un String
-- Restricciones: que las entradas sean String y una lista de String
-- Objetivo: Se encarga de calcular el total de la reservacion indicada
totalReservacion :: String -> [String] -> String
totalReservacion _ [] = "0"
totalReservacion idReserva lista
  | idReserva == idReservaLista = total
  | otherwise = totalReservacion idReserva (drop 8 lista)
  where
    idReservaLista = head lista
    total = lista !! 7

--Entrada:  Un String y una lista de String 
--Salida: Una lista String
--Restricciones: que las entradas sean String y una lista de String 
--Objetivo: Se encarga de obtener los detalles de una reservacion y los guarda en una lista

detalleReservacion::String->[String]->[String]
detalleReservacion idReserva lista = do
    let idReservaLista = head lista
    if idReserva == idReservaLista then
         saltoDeContinuos lista 9 []
    else
        detalleReservacion idReserva (arrayParserList lista 8 0)

--Entrada:  Un String y una lista
--Salida: True si esta activa, false si no
--Restricciones: que las entradas sean un String y una lista de String
--Objetivo: verifica la reservacion

isActive :: String -> [String] -> Bool
isActive _ [] = False
isActive idReserva (id:estado:_) = id == idReserva && estado == "Activa"
isActive idReserva (_:xs) = isActive idReserva xs

--------------------------------------------Consultar Facturas-----------------------------------------------------------
--Objetivo: Se encarga de leer el archivo de facturas y hacerla una lista
--Entrada: --
--Salida: --
--Restricciones: --
consultaDeFacturas:: IO()
consultaDeFacturas = do
    listaFacturas <- leerDocumento "facturas.txt"
    consultaDeFacturas2 listaFacturas

--Objetivo: Se encarga de mostrar todas las facturas
--Entrada: Una lista de String
--Salida: --
--Restricciones: --
consultaDeFacturas2 :: [String] -> IO ()
consultaDeFacturas2 [] = opcionAdmin
consultaDeFacturas2 (numFactura:idReserva:subtotal:iva:total:resto) = do
  let mensaje = "Id factura: " ++ numFactura ++ "\nTotal: " ++ total ++ "\nId reserva: " ++ idReserva ++ "\n"
  putStrLn "*******************************"
  putStrLn mensaje
  consultaDeFacturas2 resto

------------------------------menu estadisticas-------------------------------

estadistic :: IO ()
estadistic = do
    putStrLn "1.Total de Huespedes."
    putStrLn "2.Historial de habitaciones ocupadas."
    putStrLn "3.Total de habitaciones no ocupadas."
    putStrLn "4.Monto recaudado con impuestos."
    putStrLn "5.Volver."
    putStrLn  "Ingrese su opcion:"
    opc <- getLine
    case opc of
        "1" -> mostrarTotalHuespedes
        "4" -> calculoDeRecaudo
        "5" -> opcionAdmin

{--------------------------------------------------------------------------------------------------------------
--------------------------------------------Menus principales--------------------------------------------------
---------------------------------------------------------------------------------------------------------------}

------------------------------menu inicio-------------------------------

main :: IO ()
main = do
    putStrLn "1.Menu Administrativo."
    putStrLn "2.Opciones generales."
    putStrLn "3.Salir"
    putStrLn  "Ingrese su opcion:"
    opc <- getLine
    case opc of
        "1"  -> opcionAdmin
        "2" -> opcionGeneral
        "3" -> return()
        _ -> main

------------------------opciones administrativas---------------------------

opcionAdmin :: IO ()
opcionAdmin = do
    putStrLn "1.Imprimir informacion del Hotel"
    putStrLn "2.Cargar Tipo de Habitaciones"
    putStrLn "3.Asignar cantidad de habitaciones por tipo"
    putStrLn "4.Cargar Tarifas"
    putStrLn "5.Consultar reservaciones"
    putStrLn "6.Consultar Facturas"
    putStrLn "7.Estadisticas de Ocupacion"
    putStrLn "8.Volver"
    putStrLn "Ingrese su opcion:"
    opc <- getLine
    case opc of
        "1" -> imprimirInfoHotel
        "2" -> leerLosDatosHabitaciones
        "3" -> verifCargCantidades
        "4" -> leerDocTarifas
        "5" -> leerReservHistorial
        "6" -> consultaDeFacturas
        "7" -> estadistic
        "8" -> main
        _ -> opcionAdmin

------------------------------opciones generales-------------------------------

opcionGeneral :: IO ()
opcionGeneral = do
    putStrLn "1.Reservacion."
    putStrLn "2.Facturar Reservacion."
    putStrLn "3.Salir."
    putStrLn "Ingrese su opcion:"
    opc <- getLine
    case opc of
        "1" -> ralizarReserva        
        "2" -> facturar
        "3" -> main
        _ -> opcionGeneral

