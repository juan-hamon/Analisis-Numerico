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
  err = FALSE
  valores = c(0)
  
  while((xDerecha - xIzquierda) > error && !err){
    
    iteraciones = iteraciones + 1
    
    if(Fx(xIzquierda)*Fx(xDerecha) < 0){
      
      valorActual = (xDerecha + xIzquierda)/2
      valores[iteraciones] = valorActual
      
      if(Fx(valorActual) == 0){
        
        break
        
      }else{
        
        if(Fx(valorActual)*Fx(xIzquierda) > 0){
          
          xIzquierda = valorActual
          
        }else{
          
          xDerecha = valorActual
          
        }
      }
      
    }else{
      
      err = TRUE
      
    }
  }
  if(!err){
    
    valorAprox = valorActual
    itrs = seq(1, iteraciones)
    cat("Iteraciones -> ",iteraciones,"Resultado -> ",valorAprox,"\n")
    tabla = data.frame(itrs, valores)
    print(tabla)
    
  }else{
    
    cat("No se pudo encontrar la raiz")
    
  }
  
}

biseccion(0,1,10e-8)
biseccion(1,2,10e-8)
