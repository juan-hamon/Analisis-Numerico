#Remueve los elementos que estan guardados en memoria
rm(list=ls())
#Limpia la consola para una mejor visualización
cat("\014")


raiz_cuadrada = function(n, E, x){
  
  fallaValInicial = FALSE
  fallaNumNegativo = FALSE
  iteraciones = 0
  error = 0
  errores <- c(0)
  
  if(x == 0){
    
    cat("El valor inicial no puede ser 0, por favor ingrese otro dato")
    fallaValInicial = TRUE
    
  }else if(n < 0){
    
    cat("El dato para calcular la raiz no puede ser negativo")
    fallaNumNegativo = TRUE
    
  }
  if(fallaValInicial == FALSE && fallaNumNegativo == FALSE){
    
    y = ((0.5)*(x+(n/x)))
    
    while(abs(x - y) > E){
      
      iteraciones = iteraciones + 1
      error = abs(x-y)
      errores[iteraciones] = error
      x = y
      y = ((0.5)*(x+(n/x)))
      
      
    }
    itrs = seq(1,iteraciones)
    cat("Resultado = ", y," Iteraciones = ",iteraciones,"\n")
    cat("Comprobacion = ", y*y,"\n")
    tabla = data.frame(itrs, errores)
    print(tabla)
  }
  
}

raiz_cuadrada(7,10e-8,3)