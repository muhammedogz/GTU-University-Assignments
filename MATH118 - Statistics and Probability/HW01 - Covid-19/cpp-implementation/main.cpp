#include "Covid.h"
#include <iostream>

int main(void)
{
    Covid data;
    data.load_file("../owid-covid-data.csv");
    // data.load_file("test.csv");
}
