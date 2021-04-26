#include <string>
#include <vector>
#include <iostream>
#include <map>

#pragma once

using namespace std; 

/*
iso_code, continent, location, date, total_cases, new_cases, new_cases_smoothed, total_deaths, new_deaths, new_deaths_smoothed, total_cases_per_million, new_cases_per_million, new_cases_smoothed_per_million, total_deaths_per_million, new_deaths_per_million, new_deaths_smoothed_per_million, reproduction_rate, icu_patients,icu_patients_per_million, hosp_patients, hosp_patients_per_million, weekly_icu_admissions, weekly_icu_admissions_per_million, weekly_hosp_admissions, weekly_hosp_admissions_per_million, new_tests, total_tests, total_tests_per_thousand, new_tests_per_thousand, new_tests_smoothed, new_tests_smoothed_per_thousand, positive_rate, tests_per_case, tests_units, total_vaccinations, people_vaccinated, people_fully_vaccinated, new_vaccinations, new_vaccinations_smoothed, total_vaccinations_per_hundred, people_vaccinated_per_hundred, people_fully_vaccinated_per_hundred, new_vaccinations_smoothed_per_million, stringency_index, population, population_density, median_age, aged_65_older, aged_70_older, gdp_per_capita, extreme_poverty, cardiovasc_death_rate, diabetes_prevalence, female_smokers, male_smokers,handwashing_facilities, hospital_beds_per_thousand, life_expectancy, human_development_index
*/

class Covid
{
private:
    vector<map<string,string>> data;
public:
    Covid();

    void read_file(string filename);
    void print_values();

    // question #1
    int unique_country();

    // question #2
    map<string,string> first_day();

    // question 3-4-11-14-15-16
    vector<map<string, string>> find_value(string str);

    // question 5-6-7-8-9-10-12-13
    vector<map<string,string>> calculate_values(string str);


};
