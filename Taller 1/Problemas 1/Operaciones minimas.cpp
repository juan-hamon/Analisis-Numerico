#include <iostream>
#include <cmath>

int main(){
	double resultado = 0;
	double valorX = 0;
	int cantidad_coeficientes = 0;
	int cantidad_sumasOrestas = 0;
	int cantidad_operaciones = 0;
	int cantidad_multiplicaciones = 0;
	
	std::cout<<"Ingrese la cantidad de coeficientes que tiene el polinomio"<<std::endl;
	std::cin>>cantidad_coeficientes;
	
	double coeficientes[cantidad_coeficientes];
	for(int i = 0; i<cantidad_coeficientes; i++){
		std::cout<<"Ingrese el coeficiente "<<i+1<<std::endl;
		std::cin>>coeficientes[i];
	}
	std::cout<<"Ingrese el valor de X"<<std::endl;
	std::cin>>valorX;
	for(int j = 0; j<cantidad_coeficientes; j++){
		resultado = resultado*valorX + coeficientes[j];
		cantidad_sumasOrestas++;
		cantidad_multiplicaciones++;
	}
	
	
	cantidad_operaciones = cantidad_sumasOrestas + cantidad_multiplicaciones;
	std::cout<<"Resultado = "<<resultado<<std::endl;
	std::cout<<"Cantidad de sumas/restas = "<<cantidad_sumasOrestas<<std::endl;
	std::cout<<"Cantidad de multiplicaciones = "<<cantidad_multiplicaciones<<std::endl;
	std::cout<<"Cantidad de operaciones = "<<cantidad_operaciones<<std::endl;

}
