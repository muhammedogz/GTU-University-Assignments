#include "Manufacture.h"
#include <iostream>

using namespace std;

int main()
{
    Manufacture manufacture;
    manufacture.loadFile("../manufacturing_defects.txt");
    manufacture.countCases();
}