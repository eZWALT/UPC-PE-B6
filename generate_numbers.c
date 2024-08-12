#include <stdio.h>
#include <stdlib.h>



int main(int argc, char **argv) {


        int n = 10000 + 40000*atoi(argv[1]);
        
        printf("%d \n", n);

        for (int j = 0; j < n; ++j) {

            printf("%d ", rand() % n);

            if ((j+1)%25 == 0) printf("\n");

        }

        printf("\n");

    }




