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

void Covid::read_file(string filename)
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

void Covid::print_values()
{
    int i = 0;
    for (const auto& it : this->data)
    {
        cout << it.at("human_development_index");
        // for (const auto& j : it)
        // {
        //     cout << j.second;
        // }
        i++;
        cout << endl;
        if (i == 20)
            break;
    }
}

int Covid::unique_country()
{
    map<string,int> map;
    int count = 0;
    for (const auto& it : data)
    {
        string temp = it.at("location");
        if (map.find(temp) == map.end())
        {
            map[temp] = 0;
            count++;
        }
    }
    return count;
}

map<string,string> Covid::first_day()
{
    string min = "2999-01-01";
    map<string,string> map;
    for (const auto& it : data)
    {
        if (min.compare(it.at("date")) > 0)
        {
            min = it.at("date");
            map = it;
        }
    }
    return map;
}

vector<map<string, string>> Covid::find_value(string str)
{
    vector<map<string, string>> list;
    map<string, string> map;
    for (int i = 0; i < data.size() - 1; i++)
    {
        if (data[i].at(str).size() > 0)
        {    
            map[str] = data[i].at(str);
            map["location"] = data[i].at("location");
        }
        if (data[i].at("location").compare(data[i+1].at("location")) != 0)
        {
            if (map.at("location").compare(data[i].at("location")) != 0)
            {
                map["location"] = data[i].at("location");
                map[str] = "No_Info";
            }
            list.push_back(map);
        }
    }
    map["location"] = data[data.size() - 1].at("location");
    map[str] = data[data.size() - 1].at(str);
    list.push_back(map);
    return list;
}