(*  Exercise 5
    Reimplement the coloring of map functions from chapter 4, section 6
    using the higher order functions on lists *)

type Country = string 
type Map = (Country * Country) list

type Colour = Country list
type Colouring = Colour list

/// Checks whether country1 and country2 are neighbours in the given map
let areNb theMap country1 country2 =
    List.contains (country1, country2) theMap

/// Checks whether the given colour can be extended by the country in the given map
let canBeExtBy theMap theColour theCountry =
    List.forall (fun c -> not (areNb theMap c theCountry) ) theColour

/// extends the colouring by the given country
/// namely checks whether there exists a colour the can be extended
/// by the country and adds it to the color
/// or colours it in a new colour
let extColouring map colouring newCountry = 
    let extendableColour = List.tryFind (fun theColour -> canBeExtBy map theColour newCountry) colouring
    match extendableColour with
    | None -> [newCountry]::colouring
    | Some validColour -> List.map (fun colour -> if colour = validColour then newCountry::colour else colour)
                                   colouring

/// Extracts countries from a map.
let countries map = 
    let addCountries countries countryPair  = 
        match countryPair with
        | (a, b) when not (List.contains a countries) && not (List.contains b countries) -> a::b::countries
        | (a, _) when not (List.contains a countries) -> a::countries
        | (_, b) when not (List.contains b countries) -> b::countries
        | _ -> countries
    List.fold addCountries [] map

/// Colors countries on a given map.
let colCntrs map countryList = 
    List.fold (fun colouring country -> extColouring map colouring country) [] countryList

/// Colors a given map
let colMap map = colCntrs map (countries map)

    