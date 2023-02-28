# insitu_array
In Place Array Transpose for Fortran

The default transpose function in Fortran allocates a new array and copies the old array in transposed order.
This results in bad memory performance for large arrays  

This module uses a 1D array for storage and mimmics differntly shaped views to the same underlying data via pointer reassignment.
While this greatly improves memory efficiency, the underlying data array needs the `TARGET` attribute.
This might prevent the compiler from optimizing with SIMD instructions.
If this is the case, the `! omp simd` directives could be used.
