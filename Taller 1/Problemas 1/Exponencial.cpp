#include <iostream>
#include <cmath>

using namespace std;

int main()
{

	double n = exp(0.5);
	double e = 0.0001;
	double x = 0.0001;
	double y;

	y = (0.5)*(x + (n/x));

	do
	{
		x = y;
		y = (0.5)*(x + (n/x));
	}
	while((abs(x - y)) > e);


	cout<< y <<endl;

	cout<<"comprobacion : "<< y*y ;

}
