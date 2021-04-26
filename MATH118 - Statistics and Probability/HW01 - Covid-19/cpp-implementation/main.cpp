#include "Covid.h"
#include <iostream>

// Helper function for printing data
void list_print(Covid data, string str);

int main(void)
{
    Covid data;
    data.read_file("../owid-covid-data.csv");
    // data.load_file("test.csv");

    // ******* question #1 ******* 
    cout << "Country count: " <<  data.unique_country() << endl;
    // ******* question #2 ******* 
    cout << "First day: " << data.first_day().at("date") << " Country: " << data.first_day().at("location") << endl;
    // ******* question #3 *******
    list_print(data, "total_cases");
    // ******* question #4 *******
    list_print(data, "total_deaths");
    
}

void list_print(Covid data, string str)
{
    cout << "Country,"+ str << endl;
    vector<map<string,string>> value = data.find_value(str);
    for (const auto& it : value)
        cout << it.at("location") << "," << it.at(str) << endl;
}
