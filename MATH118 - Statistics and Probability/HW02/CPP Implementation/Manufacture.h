#include <map>
#include <vector>

#pragma once

using namespace std;

class Manufacture
{
public:
    void loadFile(string filename);

    void countCases();

    void calculateLambda();

    float calculatePossion(int caseNum);


private:
    // keep all data of companies
    vector< map<int,int> > data;

    // keep cases total count
    vector<int> list;

    // keep total Event
    int totalEvent = 0;

    // keep total case
    int totalCase = 0;
    
    // keep lambda result
    float lambda;
};

