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
            # i += 1
            # if (i == 500):
            #     break
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
    totalCases = total_x_per_country(data,"total_cases")
    print("\n".join("Country = {} \t\t{}: {}".format(country, string, case) for country, case in totalCases.items())) 


# question #6-7-8-9-10-12-13
def calculation_values(data, string):
    country = {}
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
    print("{} calculations shown".format(string))
    print("Country - Min - Max - Average - Variation")
    for row in value:
        print(row["location"] + " " + str(row["min"]) + " " + str(row["max"]) + " " + str(row["average"]) + " " + str(row["variation"]))



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

    

