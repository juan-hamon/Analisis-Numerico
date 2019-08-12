#Se limpian los elementos creados con anterioridad
rm(list=ls())
#Se limpia la consola para una mejor visualización 
cat("\014")

g = function(x) ((exp(1)^x)/pi)
f = function(x) (log(pi*x))


punto_fijo = function(x, error_permitido)
{
  
  iteraciones = 0
  valores = c(0)
  errores =  c(0)
  errores1 = c(0)

  while(abs(g(x) - x) > error_permitido)
  {
    
    iteraciones = iteraciones + 1
    r = (abs(g(x) -x )/g(x))
    errores[iteraciones] = r
    x = g(x)
    valores[iteraciones] = x
    if(iteraciones == 1){
      errores1[iteraciones] = 0
    }else{
      errores1[iteraciones] = errores[iteraciones-1]
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
  plot(errores, errores1 , type = "l",col="red"
       ,main = "Error en i v.s Error en i+1"
       ,xlab = "Error en i+1"
       ,ylab = "Error en i")
}

punto_fijo(0.5, 10e-8)