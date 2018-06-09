// Vector signature
module Vector
type vector
val (~-. ) : vector -> vector // Vector sign change
val ( +. ) : vector -> vector -> vector // Vector sum
val ( -. ) : vector -> vector -> vector // Vector difference
val ( *. ) : float -> vector -> vector // Product with number
val ( &. ) : vector -> vector -> float // Dot product
val norm : vector -> float // Length of vector
val make : float * float -> vector // Make vector
val coord : vector -> float * float // Get coordinates