removeMax <- function(H){
  sizeNewH <- length(H) - 1
  newH <- H
  maxVal <- H[1]
  newH[1] <- newH[sizeNewH+1]
  newH <- head(newH, -1)

  curPos <- 1
  curLeftChi <- curPos * 2
  curRightChi <- curPos * 2 + 1

  while ( curRightChi <= sizeNewH && newH[curPos] < max(newH[c(curLeftChi, curRightChi)]) ){
    if (newH[curLeftChi] >= newH[curRightChi]){
      temp <- newH[curLeftChi]
      newH[curLeftChi] <- newH[curPos]
      newH[curPos] <- temp
      curPos <- curLeftChi
    }else{
      temp <- newH[curRightChi]
      newH[curRightChi] <- newH[curPos]
      newH[curPos] <- temp
      curPos <- curRightChi
    }
    curLeftChi <- curPos * 2
    curRightChi <- curPos * 2 + 1
  }
  if (curLeftChi==sizeNewH && newH[curPos]<newH[curLeftChi] ){
    temp <- newH[curLeftChi]
    newH[curLeftChi] <- newH[curPos]
    newH[curPos] <- temp
    curPos <- curLeftChi 
  }
  return(newH)
}

H <- c(5,3,4,2,1,1)
removeMax(H)