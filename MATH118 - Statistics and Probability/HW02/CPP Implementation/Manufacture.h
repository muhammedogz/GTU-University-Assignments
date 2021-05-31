#include <map>
#include <vector>

#pragma once

using namespace std;

class Manufacture
{
public:
    void loadFile(string filename);

    void countCases();


private:
    // keep all data of companies
    vector< map<int,int> > data;

    // keep cases total count
    vector<int> list;
    
    // keep lambda result
    float lambda;
};

