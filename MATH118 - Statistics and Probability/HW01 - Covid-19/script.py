#! /usr/bin/env python3 

import csv
from datetime import datetime # for manipulating date

# question #0 load all file to memory
def load_file(f):
    data = [] # create empty list
    i = 0
    with open(f, mode="r") as file:
        csv_reader = csv.DictReader(file)
        for row in csv_reader:
            data.append(row) # append all results
    return data

# question #1
def country_count(data):
    countries = {} # create empty list
    size = 0
    for row in data:
        temp = row["location"]
        if (countries.get(temp) == None): # if not found. Increment size + 1
            countries[temp] = 0
            size += 1
    return size

# question #2
def earliest_day(data):
    earliest = datetime.now() # get time 
    earliestCountry = "" # create empty string
    for row in data:
        i = row["date"].split("-") # split date data with 3 part
        temp = datetime(int(i[0]),int(i[1]),int(i[2])) # assign values
        if temp < earliest: # if temp is smaller than, thane earliest is this value
            earliest = temp
            earliestCountry = row
    return earliestCountry
        
# question #3-4-11-14-15-16
def total_x_per_country(data, string):
    result = {} # create empty dict
    for row in data:
        if len(row[string]) > 0: # if this value is exist
            result[row["location"]] = row[string] # assign 
        elif row["location"] not in result.values():
            result[row["location"]] = "None"
    return result

# help for printing
def total_x_per_country_print(data, string):
    value = total_x_per_country(data,string)
    print("country,{}".format(string))
    print("\n".join("{},{}".format(country, case) for country, case in value.items())) 
    print("\n")

# question #6-7-8-9-10-12-13
def calculation_values(data, string):
    result = [] # create empty list
    for row in data:
        row["min"] = 0
        row["max"] = 0
        if (len(row[string]) == 0): # if not exist, continue
            continue
        temp = next((item for item in result if item["location"] == row["location"]), None) # search if this value is already exist in result list
        if (temp == None): # if not exist
            temp = row # assign with row
            result.append(temp) # append to list
            temp["count"] = 0 # create new values 
            temp["total"] = 0
            temp["min"] = float(temp[string])
            temp["max"] = float(temp[string])


        temp["count"] += 1
        temp["total"] += float(row[string])
        if (temp["max"] < float(row[string])):
            temp["max"] = float(row[string])
        if (temp["min"] > float(row[string])):
            temp["min"] = float(row[string])

    # find average value. Than calculate each values variation
    for row in result:
        row["average"] = row["total"] / row["count"]
        row["variation"] = 0

        for line in data:
            if (line["location"] == row["location"] and len(line[string]) > 0):
                row["variation"] += (float(line[string]) - row["average"]) * (float(line[string]) - row["average"]) 

        row["variation"] = row["variation"] / row["count"]
    return result
            
# helper print function
def calculation_values_print(data, string):
    value = calculation_values(data, string)
    print("Country,Min,Max,Average,Variation")
    for row in value:
        print(row["location"] + "," + str(row["min"]) + "," + str(row["max"]) + "," + str(row["average"]) + "," + str(row["variation"]))
    print("\n")


# question 17
def other_info(data):
    result = []
    for row in data:
        temp = next((item for item in result if item["location"] == row["location"]), None)
        if (temp == None):
            result.append(row)
    return result

# helper print function
def other_info_print(data):
    result = other_info(data)
    print("Country,Population,Median_Age,Older_65,Older_70,Economic,Heart_Dissase,Diabetes,Smoker_F,Smoker_M,Handwash_Hospital_Bed,Life_Expectancy,Human_Development")
    for row in result:
        print("{},{},{},{},{},{},{},{},{},{},{},{}".format(row["location"] ,row["population"], row["median_age"], row["aged_65_older"], row["aged_70_older"], row["gdp_per_capita"], row["cardiovasc_death_rate"], row["diabetes_prevalence"], row["female_smokers"], row["male_smokers"], row["handwashing_facilities"], row["hospital_beds_per_thousand"], row["life_expectancy"], row["human_development_index"]))


def list_all(data):
    result = other_info(data) # question 17
    list_all_helper2(data, result,"q3", "total_cases") 
    list_all_helper2(data, result,"q4", "total_deaths")
    list_all_helper(data, result, "q5", "reproduction_rate")
    list_all_helper(data, result, "q6", "icu_patients")
    list_all_helper(data, result, "q7", "hosp_patients")
    list_all_helper(data, result, "q8", "weekly_icu_admissions")
    list_all_helper(data, result, "q9", "weekly_hosp_admissions")
    list_all_helper(data, result, "q10", "new_tests")
    list_all_helper2(data, result,"q11", "total_tests")
    list_all_helper(data, result, "q12", "positive_rate")
    list_all_helper(data, result, "q13", "tests_per_case")
    list_all_helper2(data, result,"q14", "people_vaccinated") 
    list_all_helper2(data, result,"q15", "people_fully_vaccinated") 
    list_all_helper2(data, result,"q16", "total_vaccinations") 

    return result

def list_all_helper(data, result, name, string):
    tempList = calculation_values(data, string)
    for d in result:
        temp = [x for x in tempList if x['location'] == d['location']]

        if temp:
            temp[0][name+".min"] = temp[0].pop("min")
            temp[0][name+".max"] = temp[0].pop("max")
            temp[0][name+".avg"] = temp[0].pop("average")
            temp[0][name+".var"] = temp[0].pop("variation")
            d.update(temp[0])
        else:
            d[name+".min"] = "None"
            d[name+".max"] = "None"
            d[name+".avg"] = "None"
            d[name+".var"] = "None"
    
def list_all_helper2(data, result, name, string):
    temp = total_x_per_country(data, string)
    
    for key, value in temp.items():
        temp2 = next((item for item in result if item["location"] == key), None)
        if temp2:
            temp2[name] = value 

def list_all_print(data):
    result = list_all(data)
    print("Country,q#3,q#4,q#5_min,q#5_max,q#5_avg,q#5_var,q#6_min,q#6_max,q#6_avg,q#6_var,q#7_min,q#7_max,q#7_avg,q#7_var,q#8_min,q#8_max,q#8_avg,q#8_var,q#9_min,q#9_max,q#9_avg,q#9_var,q#10_min,q#10_max,q#10_avg,q#10_var,q#10,q#12_min,q#12_max,q#12_avg,q#12_var,q#13_min,q#13_max,q#13_avg,q#13_var,q#14,q#15,q#16,Population,Median_Age,Older_65,Older_70,Economic,Heart_Dissase,Diabetes,Smoker_F,Smoker_M,Handwash_Hospital_Bed,Life_Expectancy,Human_Development")
    for row in result:
        print("{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{}".format(row["location"],row["q3"],row["q4"],row["q5.min"],row["q5.max"],row["q5.avg"],row["q5.var"],row["q6.min"],row["q6.max"],row["q6.avg"],row["q6.var"],row["q7.min"],row["q7.max"],row["q7.avg"],row["q7.var"],row["q8.min"],row["q8.max"],row["q8.avg"],row["q8.var"],row["q9.min"],row["q9.max"],row["q9.avg"],row["q9.var"],row["q10.min"],row["q10.max"],row["q10.avg"],row["q10.var"],row["q11"] ,row["q12.min"],row["q12.max"],row["q12.avg"],row["q12.var"],row["q13.min"],row["q13.max"],row["q13.avg"],row["q13.var"],row["q14"], row["q15"],row["q16"],row["population"],row["median_age"],row["aged_65_older"],row["aged_70_older"],row["gdp_per_capita"],row["cardiovasc_death_rate"],row["diabetes_prevalence"],row["female_smokers"],row["male_smokers"],row["handwashing_facilities"],row["hospital_beds_per_thousand"],row["life_expectancy"],row["human_development_index"]))   

# load data to memory
data = load_file("owid-covid-data.csv")

# # -------------------- question 1 -------------------- 
# countryCount = country_count(data)
# print("Country Count: {}".format(countryCount))
# # -------------------- question 2 -------------------- 
# earliestDay = earliest_day(data)
# print("Earliest day in this data is: {} with this country: {}".format(earliestDay["date"],earliestDay["location"]))
# # -------------------- question 3 -------------------- 
# total_x_per_country_print(data,"total_cases")
# # -------------------- question 4 -------------------- 
# total_x_per_country_print(data,"total_deaths")
# # -------------------- question 5 -------------------- 
# calculation_values_print(data,"reproduction_rate")
# # -------------------- question 6 -------------------- 
# calculation_values_print(data,"icu_patients")
# # -------------------- question 7 -------------------- 
# calculation_values_print(data,"hosp_patients")
# # -------------------- question 8 -------------------- 
# calculation_values_print(data,"weekly_icu_admissions")
# # -------------------- question 9 -------------------- 
# calculation_values_print(data,"weekly_hosp_admissions")
# # -------------------- question 10 --------------------
# calculation_values_print(data,"new_tests")
# # -------------------- question 11 -------------------- 
# total_x_per_country_print(data,"total_tests")
# # -------------------- question 12 --------------------
# calculation_values_print(data,"positive_rate")
# # -------------------- question 13 --------------------
# calculation_values_print(data,"tests_per_case")
# # -------------------- question 14 -------------------- 
# total_x_per_country_print(data,"people_vaccinated")
# # -------------------- question 15 -------------------- 
# total_x_per_country_print(data,"people_fully_vaccinated")
# # -------------------- question 16 -------------------- 
# total_x_per_country_print(data,"total_vaccinations")
# # -------------------- question 17 -------------------- 
# other_info_print(data)
# -------------------- question 18 -------------------- 
list_all_print(data)