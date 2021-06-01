#include <map>
#include <vector>

#pragma once

using namespace std;

class Manufacture
{
public:
    // Load file from given string and keep it in memory
    void loadFile(string filename);

    // calculate each count and keep in list
    void countCases();

    // calculate lambda
    void calculateLambda();

    // Possion formula
    float calculatePossion(int caseNum);

    // calculate all cases and keep in calculatedList
    void calculateAllPossion();

    // getters
    vector<int> getList();
    vector<float> getCaluclatedList();
    float getLambda();
    int getTotalEvent();
    int getTotalCase();


private:
    // keep all data of companies
    vector< map<int,int> > data;

    // keep cases total count
    vector<int> list;

    // keep caluclated possions
    vector<float> calculatedList;

    // keep total Event
    int totalEvent = 0;

    // keep total case
    int totalCase = 0;
    
    // keep lambda result
    float lambda;
};
