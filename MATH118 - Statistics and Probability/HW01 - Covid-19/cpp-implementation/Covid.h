#include <string>
#include <vector>
#include <iostream>
#include <map>

#pragma once

static const long double MAX_VAL = -9999999999;
static const long double MIN_VAL = 9999999999;

using namespace std; 

class Covid
{
public:
    Covid();

    void read_file(string filename);
    void print_values();

    // question #1
    int unique_country();

    // question #2
    map<string,string> first_day();

    // question 3-4-11-14-15-16
    vector<map<string, string>> find_value(string& str);

    // question 5-6-7-8-9-10-12-13
    vector<map<string,string>> calculate_values(string& str);

private:
    vector<map<string,string>> data;

    // helper function for finding a value
    map<string, string> get_value_from_list(vector<map<string,string>> list, string str);
};
