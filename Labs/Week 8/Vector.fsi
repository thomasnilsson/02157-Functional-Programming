// Vector implementation
module Vector // Vector signature
type Vector = {x: float; y:float}
    val ( ~-. ) : Vector -> Vector // Vector sign change
    val ( +. ) : Vector -> Vector -> Vector // Vector sum
    val ( -. ) : Vector -> Vector -> Vector // Vector difference
    val ( *. ) : float -> Vector -> Vector // Product wth number
    val ( &. ) : Vector -> Vector -> float // Dot product
    val norm : Vector -> float // Length of vector
    val make : float * float -> Vector // Make vector
    val coord : Vector -> float * float // Get coordinates