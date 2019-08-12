#Remueve los elementos que estan guardados en memoria
rm(list=ls())
#Limpia la consola para una mejor visualización
cat("\014")

Fx = function(x) ((exp(1)^x)-(pi*x))

Steffensen = function(x0, error){
  
  iteraciones = 0
  x1 = 0
  aux = 0
  err = FALSE
  f1 = Fx(x0)
  valores = c(0)
  errores = c(0)
  errores1 = c(0)
  
  while(abs(f1) > error && err == FALSE){
    
    iteraciones = iteraciones + 1
    aux = Fx(x0 + f1) - f1
    
    if(aux == 0){
      
      cat("No se puede calcular la raiz")
      err = TRUE
      
    }else{
      
      x1 = x0-f1*f1/aux
      valores[iteraciones] = x1
      r = (abs(x0-x1)/x0)
      x0 = x1
      f1 = Fx(x0)
      errores[iteraciones] = r
      if(iteraciones == 1){
        errores1[iteraciones] = 0
      }else{
        errores1[iteraciones] = errores[iteraciones-1]
      }
    }
    
  }
  
  if(err == FALSE){
    iteracion = seq(1,iteraciones)
    tabla = data.frame(iteracion, valores)
    print(tabla)
    plot(iteracion, valores, type = "l",col="red"
         ,main = "Valores v.s Iteraciones"
         ,xlab = "Iteraciones"
         ,ylab = "Valor")
    plot(iteracion, errores, type = "l",col="red"
         ,main = "Errores v.s Iteraciones"
         ,xlab = "Iteraciones"
         ,ylab = "Error")
    plot(errores, errores1, type = "l",col="red"
         ,main = "Error en i v.s Error en i+1"
         ,xlab = "Error en i+1"
         ,ylab = "Error en i")
  }
  
}

Steffensen(0.5, 10e-8)