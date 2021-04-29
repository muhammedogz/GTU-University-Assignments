#include "Covid.h"
#include <map>
#include <vector>
#include <iostream>
#include <string>
#include <fstream>
#include <sstream>

using namespace std;

// a helper function
vector<map<string, string>>::iterator findIter(vector<map<string, string>>& data, const string & str);

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

vector<map<string, string>> Covid::find_value(string& str)
{
    vector<map<string, string>> list;
    map<string, string> map;
    int dataSize = data.size();
    map["location"] = "dummy";
    for (int i = 0; i < dataSize - 1; i++)
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
    map["location"] = data[dataSize - 1].at("location");
    map[str] = data[dataSize - 1].at(str);
    list.push_back(map);
    return list;
}

vector<map<string,string>> Covid::calculate_values(string& str)
{
    vector<map<string,string>> list;
    long double avr = 0, variance = 0, min = MIN_VAL, max = MAX_VAL, num = 0;
    double count = 0;
    int dataSize = data.size();

    for (int i = 0; i < dataSize - 1; i++)
    {
        if (data[i].at(str).size() != 0)
        {
            auto temp = stold(data[i].at(str));
            count++; avr += temp;
            if (min > temp) min = temp; 
            if (max < temp) max = temp;
        }
        if (i == dataSize - 2 || data[i].at("location").compare(data[i+1].at("location")) != 0)
        {
            avr /= count;
            data[i]["average"] = to_string(avr);  data[i]["count"] = to_string(count);
            data[i]["min"] = to_string(min); data[i]["max"] = to_string(max);
            list.push_back(data[i]);
            avr = 0; count = 0; min = MIN_VAL, max = MAX_VAL;
        }      
    }

    int listSize = list.size();
    auto dataEnd = data.end();
    for (int i = 0; i < listSize; i++)
    {    
        long double average = stold(list[i].at("average"));
        string comp = list[i]["location"];
        for (auto j = findIter(data, comp); j < dataEnd; ++j)
        {   
            if (j->at(str).size() > 0)
            {
                num = stold(j->at(str));
                variance += (num - average) * (num - average);
            }
            if (j->at("location").compare(comp) != 0)
                break;
            comp = j->at("location");
        }
        variance /= stold(list[i].at("count"));
        list[i]["variance"] = to_string(variance);
        variance = 0;
    }

    return list;
}

vector<map<string, string>>::iterator findIter(vector<map<string, string>>& data, const string & str)
{
    for (auto iter = data.begin(); iter < data.end(); ++iter)
    {
        if (iter->at("location") == str)
            return iter;
    }
    return data.end();
}

vector<map<string, string>> Covid::country_info()
{
    vector<map<string, string>> list;
    int dataSize = data.size();
    for (int i = 0; i < dataSize - 1; i++)
    {
        if (data[i].at("location") != data[i+1].at("location") || i == dataSize - 2)
            list.push_back(data[i]);
    }
    return list;
}

vector<map<string, string>> Covid::all_info()
{
    vector<map<string, string>> list = country_info();
    append_to_list(list, "q5", "reproduction_rate");
    append_to_list(list, "q6", "icu_patients");
    append_to_list(list, "q7", "hosp_patients");
    append_to_list(list, "q8", "weekly_icu_admissions");
    append_to_list(list, "q9", "weekly_hosp_admissions");
    append_to_list(list, "q10", "new_tests");
    append_to_list(list, "q12", "positive_rate");
    append_to_list(list, "q13", "tests_per_case");

    append_to_list2(list, "q3","total_cases");
    append_to_list2(list, "q4","total_deaths");
    append_to_list2(list, "q11","total_tests");
    append_to_list2(list, "q14","people_vaccinated");
    append_to_list2(list, "q15","people_fully_vaccinated");
    append_to_list2(list, "q16","total_vaccinations");

    return list;

}

void Covid::append_to_list(vector<map<string, string>>& list, string name, string str)
{
    auto append = calculate_values(str);
    for (auto it : append)
    {
        auto iter = findIter(list, it.at("location"));
        if (iter != list.end())
        {
            if (it.at("min") == to_string(MIN_VAL))
            {
                iter->insert({name+".min","No_Info"});
                iter->insert({name+".max","No_Info"});
                iter->insert({name+".avg","No_INfo"});
                iter->insert({name+".var","No_INfo"});
            }
            else
            {
                iter->insert({name+".min",it.at("min")});
                iter->insert({name+".max",it.at("max")});
                iter->insert({name+".avg",it.at("average")});
                iter->insert({name+".var",it.at("variance")});
            }        
            
        }
    }
}

void Covid::append_to_list2(vector<map<string, string>>& list, string name, string str)
{
    auto append = find_value(str);
    for (auto it : append)
    {
        auto iter = findIter(list, it.at("location"));
        if (iter != list.end())
        {
            iter->insert({name,it.at(str)});
        }
    }
}