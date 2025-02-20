/**
 * © 2024 AO Kaspersky Lab
 */

#include <stdio.h>
#include <stdlib.h>

int main(void)
{
    /* The fprintf() function prints the text to the standard error stream stderr to simplify the example.
       To work with the standard output stream stdout, a program that
       supports the VFS must be added to the solution. */
    fprintf(stderr,"Hello world!\n");

    return EXIT_SUCCESS;
}
