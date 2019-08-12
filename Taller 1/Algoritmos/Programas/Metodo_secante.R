#Remueve los elementos que estan guardados en memoria
rm(list=ls())
#Limpia la consola para una mejor visualización
cat("\014")


funcion_1 = function(x) ((exp(1)^x)-(pi*x))


secante = function(x0, x1, error_permitido)
{
  iteraciones = 0
  valores = c(0)
  errores = c(0)
  errores1 = c(0)
  
  
  repeat
  {
    iteraciones = iteraciones + 1
    x = x1 - (funcion_1(x1)*(x0 - x1))/(funcion_1(x0)-funcion_1(x1)) 
    error = error = abs((x - x1)/x)
    x0 = x1 
    x1 = x
    valores[iteraciones] = x
    errores[iteraciones] = error
    
    if(iteraciones == 1){
      errores1[iteraciones] = 0
    }
    else{
      errores1[iteraciones] = errores[iteraciones-1]
    }
    
    if(error < error_permitido){
      break
    }
    
  }
  iteracion = seq(1,iteraciones)
  tabla = data.frame(iteracion,valores)
  print(tabla)
  plot(iteracion, valores, type = "l",col="red"
       ,main = "Valores v.s Iteraciones"
       ,xlab = "Iteraciones"
       ,ylab = "Valores")
  plot(iteracion, errores, type = "l" ,col="red"
       ,main = "Errores v.s Iteraciones"
       ,xlab = "Iteraciones"
       ,ylab = "Error")
  plot(errores, errores1, type = "l",col="red"
       ,main = "Error en i v.s Error en i+1"
       ,xlab = "Error en i+1"
       ,ylab = "Error en i")
}

secante(0,1,10e-8)