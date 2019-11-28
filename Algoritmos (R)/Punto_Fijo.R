#Se limpian los elementos creados con anterioridad
rm(list=ls())
#Se limpia la consola para una mejor visualización 
cat("\014")

g = function(x) ((exp(1)^x)/pi)
f = function(x) (log(pi*x))


punto_fijo = function(x, error_permitido)
{
  
  iteraciones = 0

  while(abs(g(x) - x) > error_permitido)
  {
    
    iteraciones = iteraciones + 1
    
    x = g(x)
    
    cat("x = ", x, "\t", "Iteracion #", iteraciones, "\n")
    
  }
}

punto_fijo(0.5, 10e-8)