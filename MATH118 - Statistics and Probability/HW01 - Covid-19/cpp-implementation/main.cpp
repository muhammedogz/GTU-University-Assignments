#include "Covid.h"
#include <iostream>

// Helper function for printing find_values() function outputs
void list_print(Covid& data, string str);
// Helper function for printing calculate_values() function outputs
void list_print2(Covid& data, string str);

int main(void)
{
    Covid data;
    data.read_file("../owid-covid-data.csv");

    // ******* question #1 ******* 
    cout << "Country count: " <<  data.unique_country() << endl;
    // ******* question #2 ******* 
    cout << "First day: " << data.first_day().at("date") << " Country: " << data.first_day().at("location") << endl;
    // ******* question #3 *******
    list_print(data, "total_cases");
    // ******* question #4 *******
    list_print(data, "total_deaths");
    // ******* question #5 *******
    list_print2(data, "reproduction_rate");
    // ******* question #6 *******
    list_print2(data, "icu_patients");
    // ******* question #7 *******
    list_print2(data, "hosp_patients");
    // ******* question #8 *******
    list_print2(data, "weekly_icu_admissions");
    // ******* question #9 *******
    list_print2(data, "weekly_hosp_admissions");
    // ******* question #10 *******
    list_print2(data, "new_tests");
    // ******* question #11 *******
    list_print(data, "total_tests");
    // ******* question #12 *******
    list_print2(data, "positive_rate");
    // ******* question #13 *******
    list_print2(data, "tests_per_case");
    // ******* question #14 *******
    list_print(data, "people_vaccinated");
    // ******* question #15 *******
    list_print(data, "people_fully_vaccinated");
    // ******* question #16 *******
    list_print(data, "total_vaccinations");

}

// for calling find_values() function and printing
void list_print(Covid& data, string str)
{
    cout << "Country,"+ str << endl;
    auto value = data.find_value(str);
    for (const auto& it : value)
        cout << it.at("location") << "," << it.at(str) << endl;
    cout << endl;
}

// for calling calculate_values() function and printing
void list_print2(Covid& data, string str)
{
    auto list = data.calculate_values(str);
    cout << "Country,Min,Max,Average,Variance" << endl;
    for (const auto& it : list)
        if (it.at("min") != to_string(MIN_VAL))
            cout << it.at("location") << "," << it.at("min") << "," <<  it.at("max") <<  "," << it.at("average") << "," << it.at("variance") << endl;
    cout << endl;
}
