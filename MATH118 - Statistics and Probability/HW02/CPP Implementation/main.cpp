#include "Manufacture.h"
#include <iostream>
#include <fstream>

using namespace std;

// results for part-a
void part_a(Manufacture& manufacture);
// resutls for part-b
void part_b(Manufacture& manufacture);
// results for part-c
void part_c(Manufacture& manufacture);
// resutls for part-d
void part_d(Manufacture& manufacture);

int main()
{
    Manufacture manufacture;
    // load file to memory
    manufacture.loadFile("../manufacturing_defects.txt");
    part_a(manufacture);
    part_b(manufacture);
    part_c(manufacture);
    part_d(manufacture);

    cout << "Program Finished. Thanks For Using. Have a Good Day Hocam" << endl;
}

void part_a(Manufacture& manufacture)
{
    cout << "*** *** Part-A On Stage *** ***" << endl;
    // load all value with counting
    manufacture.countCases();
    // get list to print
    vector<int> list = manufacture.getList();

    cout << "Case \t - Count" << endl;
    for (int i = 0; i < list.size(); i++)
    {
        cout << i << "\t\t - " << list[i] << endl;
    }
}

void part_b(Manufacture& manufacture)
{
    cout << "*** *** Part-B On Stage *** ***" << endl;
    manufacture.calculateLambda();
    cout << "Lambda = " << manufacture.getLambda() << endl;
}

void part_c(Manufacture& manufacture)
{
    cout << "*** *** Part-C On Stage *** ***" << endl;
    manufacture.calculateAllPossion();

    // get data's for printing
    vector<int> list = manufacture.getList();
    vector<float> calculatedList = manufacture.getCaluclatedList();
    int cross = manufacture.getTotalCase();

    cout << "Case \t-" << "Real \t-" << "Calculated" << endl;
    for (int i = 0; i < list.size(); i++)
    {
        cout << i << "\t" << list[i] << "\t" << calculatedList[i] * cross << endl;
    }
}

void part_d(Manufacture& manufacture)
{
    cout << "*** *** Part-D On Stage *** ***" << endl;

    // File pointer to witing to file
    ofstream file;
    file.open ("part_d.csv");

    // get those values to writing
    vector<int> list = manufacture.getList();
    vector<float> calculatedList = manufacture.getCaluclatedList();
    int cross = manufacture.getTotalCase();

    // process through the csv file
    file << "Case," << "Real," << "Calculated" << endl;
    for (int i = 0; i < list.size(); i++)
    {
        file << i << "," << list[i] << "," << calculatedList[i] * cross << endl;
    }

    cout << "part_d.csv file created. Import this file to excel\nThan use barplot tool" << endl;
    cout << "I Created my barplot with this hocam. I will show this in Presentation" << endl;

    file.close();
}