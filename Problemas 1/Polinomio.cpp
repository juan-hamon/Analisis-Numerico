#include <iostream>
#include <cmath>

//Trabajo realizado por: Juan Francisco Hamón, Diego Mauricio Bulla y Juan Diego Campos

int main()
{

    int arreglo[4];
    int max_expo = 4;
    int x0 = -2;
    int cont =0;

	std::cout<<"El polinomio a trabajar es : "<<std::endl<<"2x^4 - 3x^2 + 3x - 4"<<std::endl;
	std::cout<<"En X0 = -2"<<std::endl<<"Y su cantidad de operciones minimas es : ";

	arreglo[0] = 0;
	arreglo[1] = x0;
	for(int i=2; i<=max_expo ; i++)
    {
        arreglo[i] = arreglo[i-1] * x0;
        cont++;
    }
	std::cout<<cont<<std::endl;
    std::cout<<"El resultado es : "<< 2*(arreglo[4]) + 3*(arreglo[2]) + 3*(arreglo[1]) -4;
}
