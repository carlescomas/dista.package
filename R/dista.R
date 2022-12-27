#' Function multiply the matrix d by an scalar a
#'
#' This function is a toy example to call a Fortran subroutine and create a R package
#'
#'
#' Updated 27 December 2022
#'  
#' @param x vector
#' @param y vector
#' @param a escalar
#' @param d matrix
#' @return matrix dij
#' @author Carles Comas \email{carles.comas@udl.cat}
#' @useDynLib dista.package, .registration=TRUE
#' @export

dista<-function(n,x,y,d,a){

i1<-seq(1,n,1)
distaFS <- sapply(i1, function(i1) distaS(i1, n, x, y, d, a), 
                     simplify = "array")
           distaFS<-t(distaFS)
 invisible(return(list(distaFS = distaFS, n = n, x = x, 
                          y = y, d = d, a = a)))
}

distaS<- function(i1, n, x, y, d, a){
    dij<-rep(0,n)
    storage.mode(dij) <- "double"
    out<-c()
    distaF <- .Fortran("distaf1",i1 = as.integer(i1), n = as.integer(n),
                      x = as.double(x), y = as.double(y), d = d,
                      a = as.double(a), output = (dij), PACKAGE = "dista.package")
               out=distaF$output # en aquest cas l'ouput pel valor i1 és un array (vector) de dimensió n
               return(out)
}


