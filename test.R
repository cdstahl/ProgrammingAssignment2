randMatrix <- matrix( rnorm(100), 10 )
psdMatrix <- randMatrix %*% t(randMatrix)

goodResult <- solve(psdMatrix)
source("cachematrix.R")

uut <- makeCacheMatrix(psdMatrix)


for (idx in 1:3) {
  testResult <- cacheSolve(uut)
  if ( all(testResult == goodResult) ) {
    message("Matched")
  } else {
    message("Failed")
  }
}
message("updating psdMatrix")
uut$set(psdMatrix)

for (idx in 1:3) {
  testResult <- cacheSolve(uut)
  if ( all(testResult == goodResult) ) {
    message("Matched")
  } else {
    message("Failed")
  }
}

if( all(uut$get() == psdMatrix) ) {
  message("Original data also matches")
}

