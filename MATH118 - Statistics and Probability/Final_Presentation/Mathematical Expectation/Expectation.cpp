#include <iostream>
#include "Expectation.hpp"
#include <vector>
#include <time.h>
#include <map>
#include <math.h>

using namespace std;
Expectation::Expectation()
{
    /* Initially Empty */
}

void Expectation::exampleDice(int howManyTimes)
{
    // generate new seed
    srand(time(NULL));

    int sum = 0;
    vector<int> freq = generateFreq(6);

    for (int j = 0; j < howManyTimes; j++)
    {
        int randVal = rand() % 6 + 1;
        freq[randVal - 1] += 1;
        switch (randVal)
        {
        case 2:
            sum += 20;
            break;
        case 4:
            sum += 40;
            break;
        case 6:
            sum -= 30;
            break;
        default:
            break;
        }
    }

    printVector(freq);
    cout << "Result:" <<  sum / howManyTimes << endl;
}

void Expectation::exampleAppointment(int howManyTimes)
{

    srand(time(NULL));

    vector<int> freq = generateFreq(2);

    int sum = 0;
    for (int i = 0; i < howManyTimes; i++)
    {
        int randVal = rand() % 100;
        if (randVal <= 70)
        {
            freq[0] += 1;
            sum += 1000;
        }
        
        randVal = rand() % 100;
        if (randVal <= 40)
        {
            freq[1] += 1;
            sum += 1500;
        }
    }

    printVector(freq);
    cout << "Result:" << sum / howManyTimes << endl;
}

double Expectation::meanFinder(map<double,double> m)
{
    double sum = 0;
    for (auto it : m)
    {
        sum += it.first * it.second;
    }
    cout << "Mean: " << sum << endl;
    return sum;
}

void Expectation::varianceFinder(map<double,double> m, double mean)
{
    double sum = 0;
    for (auto it : m)
    {
        double diff_square = (it.first - mean) * (it.first - mean);
        sum += diff_square * it.second;
    }
    cout << "Variance: " << sum << endl;
    cout << "Standart Derivation: " << sqrt(sum) << endl;
}

vector<int> Expectation::generateFreq(int num)
{
    vector<int> freq;
    for (int i = 0; i < num; i++)
        freq.push_back(0);

    return freq;
}

void Expectation::printVector(vector<int> vec)
{
    cout << " ------- Frequency Table -------" << endl;
    cout << "["; 
    for (int i = 0; i < vec.size(); i++)
    {
        cout << i+1 << ":" << vec[i];
        if (i != vec.size() - 1)
            cout << ", ";
    }
    cout << "]" << endl;
}

