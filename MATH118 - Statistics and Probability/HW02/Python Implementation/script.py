#! /usr/bin/env python3 


from typing import Dict, List


def load_data(f : str) -> List[Dict[str,str]]:
    data : List[Dict[str,str]] = []

    for i in range(20):
        temp : Dict[str,str] = {}
        data.append(temp)


    with open(f, mode="r") as file:
        text = file.readlines()
        for line in text:
            # get rid off newline char
            line = line.strip()
            # if line is null, skip
            if (len(line) == 0):
                continue
            # remove \n symbol at and and split by tabs
            l = line.split("\t")
            # skip first info  that shows step
            l = l[1:len(l)]

            # determine year
            year = l[0]

            # go all info and assign year values to companies
            for i in range(len(l) - 1):
                data[i][year] = l[i+1]
  


    return data

data = load_data("manufacturing_defects.txt")

for company in data:
    print(company)

