require(pracma)
#Se limpian los elementos creados con anterioridad
rm(list=ls())
#Se limpia la consola para una mejor visualización 
cat("\014")

f <- function(x){
  log(x)
}

divided.differences <- function(x, y, x0) {
  require(rSymPy)
  n <- length(x)
  q <- matrix(data = 0, n, n)
  q[,1] <- y
  f <- as.character(round(q[1,1], 5))
  fi <- ''
  
  for (i in 2:n) {
    for (j in i:n) {
      q[j,i] <- (q[j,i-1] - q[j-1,i-1]) / (x[j] - x[j-i+1])
    }
    fi <- paste(fi, '*(x - ', x[i-1], ')', sep = '', collapse = '')
    
    f <- paste(f, ' + ', round(q[i,i], 5), fi, sep = '', collapse = '')
  }
  
  x <- Var('x')
  sympy(paste('e = ', f, collapse = '', sep = ''))
  approx <- sympy(paste('e.subs(x, ', as.character(x0), ')', sep = '', collapse = ''))
  
  return(list('Approximation from Interpolation'=as.numeric(approx), 
              'Interpolated Function'=f, 
              'Divided Differences Table'=q))
}

xs = seq(1,2)
y = f(xs)

resultados <- divided.differences(xs,y,1)
resultados2 <- divided.differences(xs,y,2)

cat("Ln(x) en x = 1 \n")
print(resultados$`Approximation from Interpolation`)
print(resultados$`Interpolated Function`)
print(resultados$`Divided Differences Table`)

cat("Ln(x) en x = 2 \n")
print(resultados2$`Approximation from Interpolation`)
print(resultados2$`Interpolated Function`)
print(resultados2$`Divided Differences Table`)