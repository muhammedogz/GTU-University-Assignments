#! /usr/bin/env python3 

from typing import Dict, List


def load_data(f : str) -> List[Dict[int,int]]:
    """Load all data from file

    Args:
        f (str): Filename path/name to find file

    Returns:
        List[Dict[int,int]]: Return list od dict for keeping data
    """

    data : List[Dict[int,int]] = []

    # keep for initializing
    flag : bool = True

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
            year = int(l[0])

            # if data is not initalized, (flag == True)
            # Initialize data with length
            if flag:
                for i in range(len(l) - 1):
                    temp : Dict[int,int] = {}
                    data.append(temp)
                    flag = False

            # go all info and assign year values to companies
            for i in range(len(l) - 1):
                data[i][year] = int(l[i+1])

    return data


def case_table(data : List[Dict[int,int]]) -> List[int]:
    """Get case infos from data

    Args:
        data (List[Dict[int,int]]): Data to handled

    Returns:
        List[int]: Return total cases for all companies in a List
    """
    l : List[int] = [x * 0 for x in range(5)]

    # for each company in data
    for company in data:
        # for each case values
        for val in company.values():
            l[val] += 1
    
    return l

def find_lambda(l : List[int], show:bool = False) -> float:
    """find Lambda

    Args:
        data (List[Dict[int,int]]): Use data to find how many time
        l (List[int,int]): Use list to find total cases
    """
    # keep all events
    all_times : int = 0
    # keep total
    total : int = 0

    power : int = 0
    for num in l:
        total += num
        all_times += num * power
        power += 1

    lambda_val = float(all_times/total)

    if show:
        print("Part-b")
        print("(Events = {})/(Times = {}) = {}".format(all_times, total, lambda_val))

    return lambda_val

def part_a(l : List[int]) -> None:
    """Print answer of part a

    Args:
        l (List[int]): Used list to print
    """
    i : int = 0
    print("Part-a")
    print("Num Of Def | Total")
    for val in l:
        print("{} \t| {}".format(i,val))
        i+=1

def part_b(l : List[int]) -> None:
    find_lambda(l,True)


    

# read file and load data
data = load_data("manufacturing_defects.txt")
# load all cases
l = case_table(data)
# calculate lambda value
lambda_val = find_lambda(l)


# print part a
part_a(l)
# print part b
part_b(l)







