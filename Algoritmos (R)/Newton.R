#Remueve los elementos que estan guardados en memoria
rm(list=ls())
#Limpia la consola para una mejor visualización
cat("\014")

Funcion = function(x) ((exp(1)^x)-(pi*x))
dFX = function(x) ((exp(1)^x)-pi)

newton = function(xIzquierdo, xDerecho, error, valInicial){
  
  x = valInicial
  iteraciones = 0
  err = FALSE
  
  #Si la derivada evaluada en x es cero, no se puede continuar
  if(dFX(x) == 0){
    
    err = TRUE
    cat("No se pudo encontrar un valor aproximado")
    
  }
  if(err == FALSE){
    
    r = x - (Funcion(x)/dFX(x))
    
    while(abs(r - x) > error && err == FALSE){
      
      #Si la derivada evaluada en x es cero, no se puede continuar
      if(dFX(x) == 0){
        err = TRUE
        break
      }
      
      iteraciones = iteraciones + 1
      x = r
      r = x - (Funcion(x)/dFX(x))
      
    }
    
    cat("Iteraciones -> ", iteraciones, "Resultado aproximado -> ", x,"\n")
    
  }

}

newton(0, 1, 10e-8, 0.5)
newton(1, 2, 10e-8, 1.6)