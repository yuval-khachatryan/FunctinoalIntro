let nameAge (name, age) = 
    name + " is " + (string age) + " years old"

let even n =
    n % 2 = 0

let adjString s = if even (String.length s) 
                  then s
                  else " " + s

let weight rho = fun s -> rho * s ** 3.0
let waterWeight = weight 1000.0
let methanolWeight = weight 786.5

let pi = ref System.Math.PI
let circleArea r = !pi * r * r
circleArea 1.0
pi := 3.0
circleArea 1.0