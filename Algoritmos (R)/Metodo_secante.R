#Remueve los elementos que estan guardados en memoria
rm(list=ls())
#Limpia la consola para una mejor visualización
cat("\014")


funcion_1 = function(x) ((exp(1)^x)-(pi*x))



secante = function(x0, x1, error_permitido)
{
  iteraciones = 0
  cat("\n","Secante en ",x0,", ",x1,"\n")
  repeat
  {
    
    iteraciones = iteraciones + 1
    x = x1 - (funcion_1(x1)*(x0 - x1))/(funcion_1(x0)-funcion_1(x1)) 
    error = abs((x - x1)/x)
    x0 = x1 
    x1 = x
    
    cat("x = ", x, "\t", "Error -> ", error, "\t", "Iteacion ->", iteraciones, "\n")
    
    if(error < error_permitido){
      break
    }
    
  }
  
}

secante(0, 1, 10e-8)
secante(1, 2, 10e-8)