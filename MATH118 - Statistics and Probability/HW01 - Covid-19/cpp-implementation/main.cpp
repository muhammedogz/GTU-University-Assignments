#include "Covid.h"
#include <iostream>

// Helper function for printing find_values() function outputs
void list_print(Covid& data, string str);
// Helper function for printing calculate_values() function outputs
void list_print2(Covid& data, string str);
// Helper function for printing country_info() function outputs
void list_print3(Covid& data);
// Helper function for printing all_info() function outputs
void list_print4(Covid& data);

int main(void)
{
    Covid data;
    data.read_file("../owid-covid-data.csv");

    // // ******* question #1 ******* 
    // cout << "Country count: " <<  data.unique_country() << endl;
    // // ******* question #2 ******* 
    // cout << "First day: " << data.first_day().at("date") << " Country: " << data.first_day().at("location") << endl;
    // // ******* question #3 *******
    // list_print(data, "total_cases");
    // // ******* question #4 *******
    // list_print(data, "total_deaths");
    // // ******* question #5 *******
    // list_print2(data, "reproduction_rate");
    // // ******* question #6 *******
    // list_print2(data, "icu_patients");
    // // ******* question #7 *******
    // list_print2(data, "hosp_patients");
    // // ******* question #8 *******
    // list_print2(data, "weekly_icu_admissions");
    // // ******* question #9 *******
    // list_print2(data, "weekly_hosp_admissions");
    // // ******* question #10 *******
    // list_print2(data, "new_tests");
    // // ******* question #11 *******
    // list_print(data, "total_tests");
    // // ******* question #12 *******
    // list_print2(data, "positive_rate");
    // // ******* question #13 *******
    // list_print2(data, "tests_per_case");
    // // ******* question #14 *******
    // list_print(data, "people_vaccinated");
    // // ******* question #15 *******
    // list_print(data, "people_fully_vaccinated");
    // // ******* question #16 *******
    // list_print(data, "total_vaccinations");
    // // ******* question #17 *******
    // list_print3(data);

    
    // ******* question #18 ******* Contains All data
    list_print4(data);
    

    
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

void list_print3(Covid& data)
{
    auto list = data.country_info();
    cout << "Country,Population,Median_Age,Older_65,Older_70,Economic,Heart_Dissase,Diabetes,Smoker_F,Smoker_M,Handwash_Hospital_Bed,Life_Expectancy,Human_Development" << endl;
    for (auto row : list)
        cout << row["location"] << "," << row["population"] << "," << row["median_age"]<< "," << row["aged_65_older"]<< "," << row["aged_70_older"]<< "," << row["gdp_per_capita"]<< "," << row["cardiovasc_death_rate"]<< "," << row["diabetes_prevalence"]<< "," << row["female_smokers"]<< "," << row["male_smokers"]<< "," << row["handwashing_facilities"]<< "," << row["hospital_beds_per_thousand"]<< "," << row["life_expectancy"]<< "," << row["human_development_index"] << endl;
        
}

void list_print4(Covid& data)
{
    auto list = data.all_info();
    // cout << "size -> " << list.size() << endl;
    cout << "Country," <<
    "q#3," <<
    "q#4," << 
    "q#5_min,q#5_max,q#5_avg,q#5_var," <<
    "q#6_min,q#6_max,q#6_avg,q#6_var," <<
    "q#7_min,q#7_max,q#7_avg,q#7_var," <<
    "q#8_min,q#8_max,q#8_avg,q#8_var," <<
    "q#9_min,q#9_max,q#9_avg,q#9_var," <<
    "q#10_min,q#10_max,q#10_avg,q#10_var," <<
    "q#11," <<
    "q#12_min,q#12_max,q#12_avg,q#12_var," <<
    "q#13_min,q#13_max,q#13_avg,q#13_var," <<
    "q#14," <<
    "q#15," <<
    "q#16," <<
    "Population,Median_Age,Older_65,Older_70,Economic,Heart_Dissase,Diabetes,Smoker_F,Smoker_M,Handwash,Hospital_Bed,Life_Expectancy,Human_Development" << endl;
    for (auto row : list)
    {
        cout << row["location"]  << "," << 
        row["q3"] << "," <<
        row["q4"] << "," <<
        row["q5.min"] << "," << row["q5.max"] << ","  <<row["q5.avg"] <<"," << row["q5.var"] << "," << 
        row["q6.min"] << "," << row["q6.max"] << ","  <<row["q6.avg"] <<"," << row["q6.var"] << "," << 
        row["q7.min"] << "," << row["q7.max"] << ","  <<row["q7.avg"] <<"," << row["q7.var"] << "," << 
        row["q8.min"] << "," << row["q8.max"] << ","  <<row["q8.avg"] <<"," << row["q8.var"] << "," << 
        row["q9.min"] << "," << row["q9.max"] << ","  <<row["q9.avg"] <<"," << row["q9.var"] << "," << 
        row["q10.min"] << "," << row["q10.max"] << ","  <<row["q10.avg"] <<"," << row["q10.var"] << "," << 
        row["q11"] << "," <<
        row["q12.min"] << "," << row["q12.max"] << ","  <<row["q12.avg"] <<"," << row["q12.var"] << "," << 
        row["q13.min"] << "," << row["q13.max"] << ","  <<row["q13.avg"] <<"," << row["q13.var"] << "," << 
        row["q14"] << "," <<
        row["q15"] << "," <<
        row["q16"] << "," <<
        row["population"] << "," << row["median_age"]<< "," << row["aged_65_older"]<< "," << row["aged_70_older"]<< "," << row["gdp_per_capita"]<< "," << row["cardiovasc_death_rate"]<< "," << row["diabetes_prevalence"]<< "," << row["female_smokers"]<< "," << row["male_smokers"]<< "," << row["handwashing_facilities"]<< "," << row["hospital_beds_per_thousand"]<< "," << row["life_expectancy"]<< "," << row["human_development_index"] << endl;
    }
        
}