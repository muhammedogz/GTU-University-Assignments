#include "Covid.h"
#include "map"
#include "vector"
#include "iostream"
#include <string>
#include <fstream>
#include <sstream>

using namespace std;

Covid::Covid()
{
    // initially empty

}

void Covid::load_file(string filename)
{

    ifstream fp; // create file pointer
    fp.open(filename); // open file
    if (!fp.is_open())
    {
        cerr << "Something went wrong. File can not open. File -> " + filename << endl;
        return;
    }

    string line; // a temp line to using when parsing
    string str; // a temp string to using when parsing
    map<string, string> map; // temp map to assign values

    // handle first input of csv file to get head lines
    getline(fp, line); // get first line
    vector<string> head;
    stringstream headParse(line);
    string tempHead;
    int headItemCount = 0;
    while(getline(headParse, tempHead, ','))
    {   
        headItemCount++;
        head.push_back(tempHead);
    }


    // handle all data but head
    while (getline(fp, line))
    {
        stringstream lineParse(line);
        for (int i = 0; i < headItemCount; i++)
        {
            getline(lineParse, str, ',');
            map[head[i]] = str;
        }
        
        // add value to data
        data.push_back(map);
    }
    
}