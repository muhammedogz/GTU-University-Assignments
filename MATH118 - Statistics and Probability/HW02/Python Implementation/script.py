#! /usr/bin/env python3 

# use for calculations
import math
# use for using static type casting
from typing import Dict, List
# use for drawing barplot
import matplotlib.pyplot as plt
import numpy as np

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

def poisson_distribution(lambda_val : float, case : int) -> float:
    """Calculate possion distribution

    Args:
        lambda_val (float): lambda value
        case (int): which case will be calculated

    Returns:
        float: result
    """
    return (math.pow(lambda_val, case) * math.exp(-1 * lambda_val)) / math.factorial(case)

def calculate_all_case_possion(l : List[int], lambda_val : float, flag : bool = False) -> List[float]:
    # find total case
    total : int = 0
    for val in l:
        total += val

    case_distributions : List[float] = list()
    for i in range(len(l)):
        case_distributions.append(poisson_distribution(lambda_val, i))
        if flag:
            case_distributions[i] *= total
    
    return case_distributions


def part_a(l : List[int]) -> None:
    """Print answer of part a

    Args:
        l (List[int]): Used list to print
    """
    i : int = 0
    print("Part-a")
    print("Case \t| Total")
    for val in l:
        print("{} \t| {}".format(i,val))
        i+=1

def part_b(l : List[int]) -> None:
    """Print part b

    Args:
        l (List[int]): use list to print
    """
    find_lambda(l, show=True)

def part_c(l : List[int], lambda_val : float) -> None:
    """Print answer of part c

    Args:
        l (List[int]): Use this list
        lambda_val (float): Use this lambda value
    """
    # find total case
    total : int = 0
    for val in l:
        total += val
    
    # calculate all cases possion value
    case_distributions = calculate_all_case_possion(l, lambda_val)

    print("part-c")
    print("case \t| total\t| possion\t | poisson_distribution rate")
    for i in range(len(l)):
        print("{} \t| {}\t| {:.4f}\t| {:4f}\t".format(i, l[i], case_distributions[i] * total, case_distributions[i]))

def part_d(l : List[int]) -> None:
    """Answer to part d

    Args:
        l (List[int]): Use this list as bars1
    """
    
    # bars1 keeps all data
    bars1 = l 
    # bars2 keeps calculated possions
    bars2 = calculate_all_case_possion(l, lambda_val, True)

    # Name the table
    plt.suptitle("The actual cases and The predicted cases Table")

    # width of the bars
    barWidth = 0.3
    
    # The x position of bars
    r1 = np.arange(len(bars1))
    r2 = [x + barWidth for x in r1]
    
    # Create blue bars
    plt.bar(r1, bars1, width = barWidth, color = 'blue', edgecolor = 'black',label='Actual')
    # Create red bars
    plt.bar(r2, bars2, width = barWidth, color = 'red', edgecolor = 'black',label='Predicted')
    
    # general layout
    plt.xticks([r + barWidth for r in range(len(bars1))], ['0', '1', '2', '3', '4'])
    plt.xlabel('Cases')
    plt.ylabel('Number of Cases')

    # show legend
    plt.legend()

    # Save result to "plot.png"
    plt.savefig('plot.png',dpi=200)    

    print("Part-d")
    print("Plot created and saved to 'plot.png' file. Check it out!")


if __name__=="__main__":
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
    # print part c
    part_c(l, lambda_val)
    # create part d file
    part_d(l)







