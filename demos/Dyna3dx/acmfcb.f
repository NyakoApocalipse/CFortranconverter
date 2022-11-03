c content of .f file concealed, only instruction for manually modification is left.
c 1. result should be renamed to myresult (in collision with result keyword); 
c 2. trailing , need to be removed
c modified source code
c 1. add condition when decide between indexing a array or call a function, whose mistake used to cause vector out of bound error
c 2. insert #includes at the very beginning so that the variable(e.g., lnv) written in header/mod files will be visible before using