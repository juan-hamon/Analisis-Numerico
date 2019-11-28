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
  
  while(abs(f1) > error && err == FALSE){
    
    iteraciones = iteraciones + 1
    aux = Fx(x0 + f1) - f1
    
    if(aux == 0){
      
      cat("No se puede calcular la raiz")
      err = TRUE
      
    }else{
      
      x1 = x0-f1*f1/aux
      x0 = x1
      f1 = Fx(x0)
      
    }
    
  }
  
  if(err == FALSE){
    cat("Valor aproximado -> ", x1, "Iteraciones ->", iteraciones,"\n")
  }
  
}

Steffensen(0.5, 10e-8)
Steffensen(1.6, 10e-8)