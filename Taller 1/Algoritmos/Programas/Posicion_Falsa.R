#Se limpian los elementos creados con anterioridad
rm(list=ls())
#Se limpia la consola para una mejor visualización 
cat("\014")

Fx = function(x) ((exp(1)^x)-(pi*x))
dFx = function(x) ((exp(1)^x)-pi)

posicionFalsa = function(xIzquierdo, xDerecho, error){
  
  iteraciones = 0
  errors = 1
  valores = c(0)
  errores = c(0)
  errores1 = c(0)
  
  while(errors > error){
    
    iteraciones = iteraciones + 1
    x = (Fx(xDerecho)*xIzquierdo - Fx(xIzquierdo)*xDerecho)/(Fx(xDerecho)-Fx(xIzquierdo))
    valores[iteraciones] = x
    
    if(Fx(x) == 0){
      break
    }
    
    if(Fx(x)*Fx(xIzquierdo) < 0){
      xDerecho = x
    }
    
    else{
      xIzquierdo = x
    }
    
    errors = abs(Fx(x)/dFx(x))
    errores[iteraciones] = errors
    if(iteraciones == 1){
      errores1[iteraciones] = 0
    }
    else{
      errores1[iteraciones] = errores[iteraciones-1]
    }
    
  }
  iteracion = seq(1,iteraciones)
  tabla = data.frame(iteracion,valores)
  print(tabla)
  plot(iteracion, valores, type = "l",col="blue"
       ,main = "Valores v.s Iteraciones"
       ,xlab = "Iteraciones"
       ,ylab = "Valor")
  plot(iteracion, errores, type = "l",col="red"
       ,main = "Errores v.s Iteraciones"
       ,xlab = "Iteraciones"
       ,ylab = "Error")
  plot(errores,errores1, type = "l", col= "red"
       ,main = "Errores en i v.s Errores en i+1"
       ,xlab = "Error en i+1"
       ,ylab = "Error en i")
}

posicionFalsa(0, 1, 10e-8)