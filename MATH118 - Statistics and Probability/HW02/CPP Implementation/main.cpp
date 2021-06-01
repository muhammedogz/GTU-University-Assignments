#include "Manufacture.h"
#include <iostream>

using namespace std;

// results for part-a
void part_a();

int main()
{
    Manufacture manufacture;
    manufacture.loadFile("../manufacturing_defects.txt");
    part_a();
    
}

void part_a(Manufacture manufacture)
{
    // load all value with counting
    manufacture.countCases();

    vector<int> list = manufacture.getList();

    cout << "Case \t - Count" << endl;
    for (int i = 0; i < list.size(); i++)
    {
        cout << i << "\t - " << list[i] << endl;
    }
}