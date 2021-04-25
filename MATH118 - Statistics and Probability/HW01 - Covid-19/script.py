#! /usr/bin/env python3 

import csv
from datetime import datetime

# question #0 load all file to memory
def load_file(f):
    data = []
    i = 0
    with open(f, mode="r") as file:
        csv_reader = csv.DictReader(file)
        for row in csv_reader:
            data.append(row)
    return data

# question #1
def country_count(data):
    countries = {}
    size = 0
    for row in data:
        temp = row["location"]
        if (countries.get(temp) == None):
            countries[temp] = 0
            size += 1
    return size

# question #2
def earliest_day(data):
    earliest = datetime.now()
    earliestCountry = ""
    for row in data:
        i = row["date"].split("-")
        temp = datetime(int(i[0]),int(i[1]),int(i[2]))
        if temp < earliest:
            earliest = temp
            earliestCountry = row
    return earliestCountry
        
# question #3-4-11-14-15-16
def total_x_per_country(data, string):
    result = {}
    for row in data:
        if len(row[string]) > 0:
            result[row["location"]] = row[string]
    return result

def total_x_per_country_print(data, string):
    value = total_x_per_country(data,string)
    print("country,{}".format(string))
    print("\n".join("{},{}".format(country, case) for country, case in value.items())) 

# question #6-7-8-9-10-12-13
def calculation_values(data, string):
    result = []
    for row in data:
        if (len(row[string]) == 0):
            continue
        temp = next((item for item in result if item["location"] == row["location"]), None)
        if (temp == None):
            temp = row
            result.append(temp)
            temp["count"] = 0
            temp["total"] = 0
            temp["min"] = float(temp[string])
            temp["max"] = float(temp[string])


        temp["count"] += 1
        temp["total"] += float(row[string])
        if (temp["max"] < float(row[string])):
            temp["max"] = float(row[string])
        if (temp["min"] > float(row[string])):
            temp["min"] = float(row[string])


    for row in result:
        row["average"] = row["total"] / row["count"]
        row["variation"] = 0

        for line in data:
            if (line["location"] == row["location"] and len(line[string]) > 0):
                row["variation"] += (float(line[string]) - row["average"]) * (float(line[string]) - row["average"]) 

        row["variation"] = row["variation"] / row["count"]
    return result
            
def calculation_values_print(data, string):
    value = calculation_values(data, string)
    print("Country,Min,Max,Average,Variation")
    for row in value:
        print(row["location"] + "," + str(row["min"]) + "," + str(row["max"]) + "," + str(row["average"]) + "," + str(row["variation"]))


# question 17
def other_info(data):
    result = []
    for row in data:
        temp = next((item for item in result if item["location"] == row["location"]), None)
        if (temp == None):
            result.append(row)
    return result

def other_info_print(data):
    result = other_info(data)
    print("Country,Population,Median_Age,Older_65,Older_70,Economic,Heart_Dissase,Diabetes,Smoker_F,Smoker_M,Handwash_Hospital_Bed,Life_Expectancy,Human_Development")
    for row in result:
        print("{},{},{},{},{},{},{},{},{},{},{},{}".format(row["location"] ,row["population"], row["median_age"], row["aged_65_older"], row["aged_70_older"], row["gdp_per_capita"], row["cardiovasc_death_rate"], row["diabetes_prevalence"], row["female_smokers"], row["male_smokers"], row["handwashing_facilities"], row["hospital_beds_per_thousand"], row["life_expectancy"], row["human_development_index"]))




def list_all(data):
    result = other_info(data) # question 17
    list_all_helper2(data, result, "total_cases") # question 3
    list_all_helper2(data, result, "total_deaths") # question 4
    list_all_helper(data, result, "q5", "reproduction_rate")
    list_all_helper(data, result, "q6", "icu_patients")
    list_all_helper(data, result, "q7", "hosp_patients")
    list_all_helper(data, result, "q8", "weekly_icu_admissions")
    list_all_helper(data, result, "q9", "weekly_hosp_admissions")
    list_all_helper(data, result, "q10", "new_tests")
    list_all_helper2(data, result, "total_tests") # question 11
    list_all_helper(data, result, "q12", "positive_rate")
    list_all_helper(data, result, "q13", "tests_per_case")
    list_all_helper2(data, result, "people_vaccinated") # question 14
    list_all_helper2(data, result, "people_fully_vaccinated") # question 15
    list_all_helper2(data, result, "total_vaccinations") # question 16


    print("Country,total_cases,total_deaths,reproduction_rate,icu_patients,hosp_patients,weekly_icu_admissions,weekly_hosp_admissions,new_tests,total_tests,positive_rate,tests_per_case,people_vaccinated,total_vaccinations,Population,Median_Age,Older_65,Older_70,Economic,Heart_Dissase,Diabetes,Smoker_F,Smoker_M,Handwash_Hospital_Bed,Life_Expectancy,Human_Development")
    for row in result:
        print("{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{}".format(row["location"] ,row["total_cases"] ,row["total_deaths"] ,row["reproduction_rate"] ,row["icu_patients"] ,row["hosp_patients"] ,row["weekly_icu_admissions"] ,row["weekly_hosp_admissions"] ,row["new_tests"] ,row["total_tests"] ,row["positive_rate"] ,row["tests_per_case"] ,row["people_vaccinated"] ,row["people_fully_vaccinated"] ,row["total_vaccinations"] ,row["population"], row["median_age"], row["aged_65_older"], row["aged_70_older"], row["gdp_per_capita"], row["cardiovasc_death_rate"], row["diabetes_prevalence"], row["female_smokers"], row["male_smokers"], row["handwashing_facilities"], row["hospital_beds_per_thousand"], row["life_expectancy"], row["human_development_index"]))
        

def list_all_helper(data, result, name, string):
    tempList = calculation_values(data, string)
    for d in result:
        temp = [x for x in tempList if x['location'] == d['location']]

        if temp:
            temp[0][name+".min"] = temp[0].pop("min")
            temp[0][name+".max"] = temp[0].pop("max")
            temp[0][name+".avr"] = temp[0].pop("average")
            temp[0][name+".var"] = temp[0].pop("variation")
            d.update(temp[0])
    
def list_all_helper2(data, result, string):
    temp = total_x_per_country(data, string)
    
    for key, value in temp.items():
        temp2 = next((item for item in result if item["location"] == key), None)
        if temp2:
            temp2[string] = value 

# load data to memory
data = load_file("owid-covid-data.csv")

# -------------------- question 1 -------------------- 
countryCount = country_count(data)
print("County Count {}".format(countryCount))
# -------------------- question 2 -------------------- 
earliestDay = earliest_day(data)
print("Earliest day in this data is {} with this country {}".format(earliestDay["date"],earliestDay["location"]))
# -------------------- question 3 -------------------- 
total_x_per_country_print(data,"total_cases")
# -------------------- question 4 -------------------- 
total_x_per_country_print(data,"total_deaths")
# -------------------- question 5 -------------------- 
calculation_values_print(data,"reproduction_rate")
# -------------------- question 6 -------------------- 
calculation_values_print(data,"icu_patients")
# -------------------- question 7 -------------------- 
calculation_values_print(data,"hosp_patients")
# -------------------- question 8 -------------------- 
calculation_values_print(data,"weekly_icu_admissions")
# -------------------- question 9 -------------------- 
calculation_values_print(data,"weekly_hosp_admissions")
# -------------------- question 10 --------------------
calculation_values_print(data,"new_tests")
# -------------------- question 11 -------------------- 
total_x_per_country_print(data,"total_tests")
# -------------------- question 12 --------------------
calculation_values_print(data,"positive_rate")
# -------------------- question 13 --------------------
calculation_values_print(data,"tests_per_case")
# -------------------- question 14 -------------------- 
total_x_per_country_print(data,"people_vaccinated")
# -------------------- question 15 -------------------- 
total_x_per_country_print(data,"people_fully_vaccinated")
# -------------------- question 16 -------------------- 
total_x_per_country_print(data,"total_vaccinations")
# -------------------- question 17 -------------------- 
other_info_print(data)
# -------------------- question 18 -------------------- 
list_all(data)