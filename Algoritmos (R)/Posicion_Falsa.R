#Se limpian los elementos creados con anterioridad
rm(list=ls())
#Se limpia la consola para una mejor visualización 
cat("\014")

Fx = function(x) ((exp(1)^x)-(pi*x))
dFx = function(x) ((exp(1)^x)-pi)

posicionFalsa = function(xIzquierdo, xDerecho, error){
  
  iteraciones = 0
  errors = 1
  
  cat("\n","Posicion falsa en ",xIzquierdo,", ",xDerecho,"\n")
  
  while(errors > error){
    
    iteraciones = iteraciones + 1
    x = (Fx(xDerecho)*xIzquierdo - Fx(xIzquierdo)*xDerecho)/(Fx(xDerecho)-Fx(xIzquierdo))
    
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
    cat("X -> ",x,"\t","E -> ", errors,"\t","Iteracion -> ",iteraciones,"\n")
    
  }
  
}

posicionFalsa(0, 1, 10e-8)
posicionFalsa(1, 2, 10e-8)