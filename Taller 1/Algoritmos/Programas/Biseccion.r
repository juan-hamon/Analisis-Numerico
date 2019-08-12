#Remueve los elementos que estan guardados en memoria
rm(list=ls())
#Limpia la consola para una mejor visualización
cat("\014")

Fx = function(x) ((exp(1)^x)-(pi*x))

biseccion = function(izquierdo, derecho, error){
  
  xIzquierda = izquierdo
  xDerecha = derecho
  iteraciones = 0
  valorAprox = 0
  valorActual = 0
  errorp = 0
  err = FALSE
  valores = c(0)
  errores = c(0)
  errores1 = c(0)
  
  while((xDerecha - xIzquierda) > error && !err){
    
    iteraciones = iteraciones + 1
    
    if(Fx(xIzquierda)*Fx(xDerecha) < 0){
      
      valorActual = (xDerecha + xIzquierda)/2
      valores[iteraciones] = valorActual
      
      if(Fx(valorActual) == 0){
        
        break
        
      }else{
        
        if(Fx(valorActual)*Fx(xIzquierda) > 0){
          errorp = abs((valorActual-xIzquierda)/valorActual)
          errores[iteraciones] = errorp
          if(iteraciones == 1){
            errores1[iteraciones] = 0
          }else{
            errores1[iteraciones] = errores[iteraciones-1]
          }
          xIzquierda = valorActual
          
        }else{
          errorp = abs((valorActual-xDerecha)/valorActual)
          errores[iteraciones] = errorp
          if(iteraciones == 1){
            errores1[iteraciones] = 0
          }else{
            errores1[iteraciones] = errores[iteraciones-1]
          }
          xDerecha = valorActual
          
        }
      }
      
    }else{
      
      err = TRUE
      
    }
  }
  if(!err){
    
    valorAprox = valorActual
    iteracion = seq(1, iteraciones)
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
    plot(errores, errores1, type = "l", col="red"
         ,main = "Error en i v.s Error en i+1"
         ,xlab = "Error en i+1"
         ,ylab = "Error en i")
    
  }else{
    
    cat("No se pudo encontrar la raiz")
    
  }
  
}

biseccion(0,1,10e-8)