#Remueve los elementos que estan guardados en memoria
rm(list=ls())
#Limpia la consola para una mejor visualización
cat("\014")

Funcion = function(x) ((exp(1)^x)-(pi*x))
dFX = function(x) ((exp(1)^x)-pi)

newton = function(error, valInicial){
  
  x = valInicial
  iteraciones = 0
  err = FALSE
  rela = 0
  valores = c(0)
  errores = c(0)
  errores1 = c(0)
  
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
      rela = (abs(r - x)/r)
      errores[iteraciones] = rela
      valores[iteraciones]= x
      if(iteraciones == 1){
        errores1[iteraciones] = 0
      }else{
        errores1[iteraciones] = errores[iteraciones-1]
      }
      
    }
    
  }
  iteracion = seq(1,iteraciones)
  tabla = data.frame(iteracion,valores)
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

newton(10e-8, 0.5)