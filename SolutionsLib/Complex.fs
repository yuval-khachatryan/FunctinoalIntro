namespace Exercise2

module Complex =
    type Complex = 
        {Real: float; Imaginary: float}
    
    let conjugate {Real = re; Imaginary = im} = {Real = re; Imaginary = -im}
    type Complex with
        static member (~- ) {Real = a; Imaginary = b} = {Real = -a; Imaginary = -b}
        static member ( + ) ({Real = a; Imaginary = b}, {Real = c; Imaginary = d} ) =
            {Real = a + c; Imaginary = b + d}
        static member ( - ) ({Real = a; Imaginary = b}, {Real = c; Imaginary = d} ) =
            {Real = a - c; Imaginary = b - d}
        static member ( * ) ({Real = a; Imaginary = b}, {Real = c; Imaginary = d}) =
            {Real = a * c - b * d; Imaginary = a * d + b * c}
        static member ( / ) ({Real = a; Imaginary = b}, {Real = c; Imaginary = d}) = 
            if c <> 0.0 || d <> 0.0 
            then {Real = a * c + b * d; Imaginary = a * (-d) + b * c}
            else failwith "Division by Zero"
    let modulus {Real = re; Imaginary = im} = System.Math.Sqrt(re * re + im * im)
    let argument {Real = re; Imaginary = im} = 
        let angle = System.Math.Atan2(im, re) 
        if angle < 0.0 then angle + 2.0 * System.Math.PI else angle
