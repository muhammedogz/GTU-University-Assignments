#include "Manufacture.h"
#include <map>
#include <iostream>
#include <vector>
#include <sstream>
#include <fstream>

using namespace std;

void Manufacture::loadFile(string loadFile)
{
    ifstream fp; // create file pointer
    fp.open(loadFile); // open file
    if (!fp.is_open())
    {
        cerr << "Something went wrong. File can not open. File -> " + loadFile << endl;
        return;
    }

    string line;
    int i = 0;
    while (getline(fp, line, '\t'))
    {
        if (i++ == 10) break;
        cout << line << endl;
    }
}