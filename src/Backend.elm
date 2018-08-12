module Backend exposing(..)
import Models exposing(Movie, Preferences)
import List exposing (map, filter, any)
import String exposing (contains, toLower, words)


completaAca = identity

-- **************
-- Requerimiento: filtrar películas por su título a medida que se escribe en el buscador;
-- **************

filtrarPeliculasPorPalabrasClave : String -> List Movie -> List Movie
filtrarPeliculasPorPalabrasClave palabras = List.filter (peliculaTienePalabrasClave palabras)

peliculaTienePalabrasClave : String -> Movie -> Bool
peliculaTienePalabrasClave palabras pelicula = List.any ( flip String.contains (toLower pelicula.title) <<toLower) (words palabras)


-- esta función la dejamos casi lista, pero tiene un pequeño bug. ¡Corregilo!
--
-- Además tiene dos problemas, que también deberías corregir:
--
-- * distingue mayúsculas de minúsculas, pero debería encontrar a "Lion King" aunque escriba "kINg"
-- * busca una coincidencia exacta, pero si escribís "Avengers Ultron" debería encontrar a "Avengers: Age Of Ultron"
--


-- **************
-- Requerimiento: visualizar las películas según el género elegido en un selector;
-- **************


filtrarPeliculasPorGenero : String -> List Movie -> List Movie
filtrarPeliculasPorGenero genero = List.filter (mismoGenero genero)

mismoGenero : String -> Movie -> Bool
mismoGenero genero pelicula = List.member (toLower genero) (map toLower pelicula.genre)


-- **************
-- Requerimiento: filtrar las películas que sean aptas para menores de edad,
--                usando un checkbox;
-- **************

filtrarPeliculasPorMenoresDeEdad : Bool -> List Movie -> List Movie
filtrarPeliculasPorMenoresDeEdad soloParaMenores = List.filter (peliculasAptaPara soloParaMenores)

peliculasAptaPara : Bool -> Movie -> Bool
peliculasAptaPara soloParaMenores pelicula = soloParaMenores && pelicula.forKids

-- **************
-- Requerimiento: ordenar las películas por su rating;
-- **************

ordenarPeliculasPorRating : List Movie -> List Movie
ordenarPeliculasPorRating =  List.reverse << List.sortBy .rating

-- **************
-- Requerimiento: dar like a una película
-- **************

darLikeAPelicula : Int -> List Movie -> List Movie
darLikeAPelicula id = List.map (peliculaConLike id)

peliculaConLike : Int -> Movie -> Movie
peliculaConLike id pelicula = if (id == pelicula.id) then 
                                  {pelicula | likes = pelicula.likes + 1} 
                              else 
                                  pelicula


-- **************
-- Requerimiento: cargar preferencias a través de un popup modal,
--                calcular índice de coincidencia de cada película y
--                mostrarlo junto a la misma;
-- **************

calcularPorcentajeDeCoincidencia : Preferences -> List Movie -> List Movie
calcularPorcentajeDeCoincidencia preferencias = List.map (cambiarPorcentaje preferencias)


cambiarPorcentaje : Preferences -> Movie -> Movie
cambiarPorcentaje preferencias  = noSuperar100Porciento << (porcentajeGeneroPredilecto preferencias.genre) << (porcentajeActor preferencias.favoriteActor) << (porcentajePalabrasClave preferencias.keywords)

noSuperar100Porciento : Movie -> Movie
noSuperar100Porciento pelicula = if pelicula.matchPercentage > 100 then
                                     {pelicula | matchPercentage = 100}
                                 else
                                     pelicula

aumentarPorcentaje : Movie -> Int -> Movie
aumentarPorcentaje pelicula aumento = {pelicula | matchPercentage = pelicula.matchPercentage + aumento}

porcentajeGeneroPredilecto : String -> Movie -> Movie
porcentajeGeneroPredilecto genero pelicula =
    if mismoGenero genero pelicula then
        aumentarPorcentaje pelicula 60
    else
        pelicula

porcentajeActor : String -> Movie -> Movie
porcentajeActor actor pelicula =
    if peliculaTieneActor actor pelicula then
        aumentarPorcentaje pelicula 50
    else
        pelicula 

peliculaTieneActor : String -> Movie -> Bool
peliculaTieneActor actor pelicula = List.member actor pelicula.actors

porcentajePalabrasClave : String -> Movie -> Movie
porcentajePalabrasClave palabras pelicula = aumentarPorcentaje pelicula (List.sum (List.map (obtenerPorcentajePalabraClave (toLower pelicula.title)) (List.map toLower (String.words palabras))))

obtenerPorcentajePalabraClave : String -> String -> Int
obtenerPorcentajePalabraClave pelicula palabraClave = 
    if any ((==) palabraClave) (words pelicula) then
        20
    else
        0

--porcentajeGenerosSimilares : Preferences -> Movie -> Movie
--porcentajeGenerosSimilares preferencias pelicula = 