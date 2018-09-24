// Solution to exercise 21, Chapter 4, Functional Programming with F#
// Same as 20, however handles maps with countries that have bi neighbours
// achieved by slightly changing the Map class... Another approach is to define the map as a graph

type Country = string
type Map = {Countries: Country list;
            Neighbours: (Country * Country) list}
type Color = Country list
type Coloring = Color list

let rec isMember elem lst = 
    match lst with
    | [] -> false
    | x::xs when x = elem -> true
    | _::xs -> isMember elem lst

let rec extractElements pairs = 
    let addElem x ys = if isMember x ys then ys else (x::ys) 
    match pairs with
    | [] -> []
    | (c1, c2)::c -> addElem c1 (addElem c2 (extractElements c))

let isValidMap map =
    let countries = map.Countries
    let neighbours = map.Neighbours
    let rec elementsSubset lst1 lst2 = 
        match lst1  with
        | [] -> true
        | x::xs -> isMember x lst2 && elementsSubset xs lst2
    elementsSubset (extractElements map.Neighbours) map.Countries

// Exercise 21
let colMap map =
    // checks whether two countries are neighbors in the map
    let areNb country1 country2 = isMember (country1, country2) map.Neighbours || isMember (country2, country1) map.Neighbours

    // checks whehter country has a neighour of given colour
    let rec canBeExtBy colour country = 
        match colour with
        | [] -> true
        | country'::countries -> not (areNb country country') && (canBeExtBy countries country)
    
    // extends the colouring
    let rec extColouring colouring country =
        match colouring with
        | [] -> [[country]]
        | colour::colours -> if canBeExtBy colour country
                             then (country::colour)::colours
                             else colour::(extColouring colours country)
    
    // colours the countries
    let rec colCountries = function
        | [] -> []
        | country::countries -> extColouring (colCountries countries) country
    
    if not (isValidMap map) 
    then failwith "Invalid map" 
    else colCountries map.Countries