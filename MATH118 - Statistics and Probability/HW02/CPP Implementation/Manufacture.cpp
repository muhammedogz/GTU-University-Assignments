#include "Manufacture.h"
#include <map>
#include <iostream>
#include <vector>
#include <sstream>
#include <fstream>
#include <math.h>

using namespace std;

// a helper function for taking factorial
long int factorial(int num)
{
    if (num <= 1) return 1;
    else return num * factorial(num-1);
}

void Manufacture::loadFile(string loadFile)
{
    ifstream fp; // create file pointer
    fp.open(loadFile); // open file
    if (!fp.is_open())
    {
        cerr << "Something went wrong. File can not open. File -> " + loadFile << endl;
        return;
    }

    // use this for keeping lines
    string line;
    // use this if data is not initialized yet
    bool initialize = true;

    // iterate over file by reading line by line
    while (getline(fp, line))
    {
        // if line length equal to zero, continue
        if (line.length() == 0) 
            continue;

        // create a row vector
        vector<int> row;

        // use stringstream to parse string
        stringstream temp(line);
        
        // use this for each parsed string 
        string str;

        // skip first line
        bool firstTime = false;

        // read line by delimeter tab
        while(getline(temp, str, '\t'))
        {
            if (!firstTime)
            {
                firstTime = true;
                continue;
            }

            row.push_back(atoi(str.c_str()));
        }    

        // initialize data value if not initialized yet
        if (initialize)
        {
            for (long unsigned int i = 0; i < row.size(); i++)
            {
                data.push_back(map<int,int>());
            }
            initialize = false;
        } 

        // keep year value
        int year = row[0];

        // use a loop to read cases and assign with companies
        for (long unsigned int i = 0; i < row.size() - 1; i++)
        {
            data[i][year] = row[i+1];
        }
    }

    // void function
    // end of the function, data value will fill with companies and their cases

}


void Manufacture::countCases()
{
    // there is 5 cases. So
    // append 5 value with 0
    for (int i = 0; i < 5; i++)
        list.push_back(0);

    // iterate all over data
    for (const auto company : data)
    {
        // iterate all over company 
        for (const auto val : company)
        {
            // use key-value pair and use val.second to assign
            list[val.second]++;
            totalEvent += val.second;
            totalCase++;
        }
    }
}

void Manufacture::calculateLambda()
{
    // divide all events to all case gives us mean (lambda) value
    lambda = (float) totalEvent / (float) totalCase;
}

float Manufacture::calculatePossion(int caseNum)
{
    // formula for calculation
    return (pow(lambda, caseNum) * exp(-1 * lambda)) / factorial(caseNum);
}

void Manufacture::calculateAllPossion()
{
    // there is 5 case, So
    // go from 0 to 5
    for (int i = 0; i < 5; i++)
        this->calculatedList.push_back(calculatePossion(i));
}


// --- Getters
vector<int> Manufacture::getList()
{
    return this->list;
}
vector<float> Manufacture::getCaluclatedList()
{
    return this->calculatedList;
}
float Manufacture::getLambda()
{
    return this->lambda;
}
int Manufacture::getTotalCase()
{
    return this->totalCase;
}
int Manufacture::getTotalEvent()
{
    return this->totalEvent;
}



