/* boxmuller.c           Implements the Polar form of the Box-Muller
                         Transformation

                      (c) Copyright 1994, Everett F. Carter Jr.
                          Permission is granted by the author to use
			  this software for any application provided this
			  copyright notice is preserved.

*/

#include <math.h>
#include <stdio.h>
#include <algorithm>
#include <time.h>


//extern float ranf();         /* ranf() is uniform in 0..1 */

float ranf(float max)
{
  return (((float)rand())/RAND_MAX)*max;
}


float box_muller(float m, float s)	/* normal random variate generator */
{				        /* mean m, standard deviation s */
	float x1, x2, w, y1;
	static float y2;
	static int use_last = 0;

	if (use_last)		        /* use value from previous call */
	{
		y1 = y2;
		use_last = 0;
	}
	else
	{
		do {
			x1 = 2.0 * ranf(1.0) - 1.0;
			x2 = 2.0 * ranf(1.0) - 1.0;
			w = x1 * x1 + x2 * x2;
		} while ( w >= 1.0 );

		w = sqrt( (-2.0 * log( w ) ) / w );
		y1 = x1 * w;
		y2 = x2 * w;
		use_last = 1;
	}

	return( m + y1 * s );
}

int main()
{
        /* initialize random seed: */
        srand(time(NULL));

        float num;
        num = box_muller(0.0,1.0);
        printf("%f\n",num);

        FILE *f = fopen("file.txt", "w");

        float a[100];
        for (int i = 0; i<100; i++)
        {
            a[i] = box_muller(0.0,1.0);
            fprintf(f,"%d %f",i,a[i]);
            fprintf(f,"\n");
        }
        
        fclose(f);
        return(0);
}



