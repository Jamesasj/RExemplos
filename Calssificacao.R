normal <- function(x, media, desvio) {
  res <- pnorm(x, media, desvio)
  print(res)
  if((res < 0.05) | res >(0.95)){
    return(0)  
  }else{
    return(res)
  }
}

classificador <- function(x){
  cT <- normal(x, 10, 6)
  cI <- normal(x, 100, 10)
  cD <- normal(x, 500, 4)
  cC <- normal(x, 200, 8)
  cN <- normal(x, 1, 1)
  probab = c(cT, cI, cD, cC, cN)
  print(probab)
  maior <- max(probab)
  print(maior)
  if (maior == 0){
    return("NA")
  }
  if (maior == cT){
    return("T")
  }
  if (maior == cI){
    return("I")
  }
  if (maior == cD){
    return("D")
  }
  if (maior == cC){
    return("C")
  }
  return("NA")
}

h <- classificador(100)