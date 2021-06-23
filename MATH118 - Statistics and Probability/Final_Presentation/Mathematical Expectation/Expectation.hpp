#include <iostream>
#include <vector>

#pragma once

class Expectation
{
public:
    Expectation(/* args */);
    
    int calculateExpectation(int howManyTimes);

private:
    /* data */
    std::vector<int> freq_dice;

    /* private functions */
    std::vector<int> generateFreq(int size);
    void printVector(std::vector<int> vec);
};


