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
            for (int i = 0; i < row.size(); i++)
            {
                data.push_back(map<int,int>());
            }
            initialize = false;
        } 

        // keep year value
        int year = row[0];

        // use a loop to read cases and assign with companies
        for (int i = 0; i < row.size() - 1; i++)
        {
            data[i][year] = row[i+1];
        }
    }

    // void function
    // end of the function, data value will fill with companies and their cases

}