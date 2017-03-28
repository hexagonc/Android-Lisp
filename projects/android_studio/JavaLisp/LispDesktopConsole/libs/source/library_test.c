#include <stdio.h>
#include "neural_library.h"
#include "tools.h"



int main(int argc, char **argv)
{
	double ran = randomLCG();

	for (int i = 0;i < 100; i++)
	{
		ran = randomLCG();
		printf("Random value %f\n", ran);
	}
	return 0;
}