#! /usr/bin/env python3 

import csv
from datetime import datetime

def load_file(f):
    data = []
    i = 0
    with open(f, mode="r") as file:
        csv_reader = csv.DictReader(file)
        for row in csv_reader:
            data.append(row)
            # i += 1
            # if (i == 100):
            #     break
    return data

def country_count(data):
    countries = {}
    size = 0
    for row in data:
        temp = row["location"]
        if (count.get(temp) == None):
            countries[temp] = 0
            size += 1
    return size

def earliest_day(data):
    earliest = datetime.now()
    
    for row in data:
        i = row["date"].split("-")
        temp = datetime(int(i[0]),int(i[1]),int(i[2]))
        if temp < earliest:
            earliest = temp
    print(earliest)
        

data = load_file("owid-covid-data.csv")
# print("County Count {}".format(country_count(data)))
earliest_day(data)

# print(data)
