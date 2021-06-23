#include <iostream>
#include "Expectation.hpp"

using namespace std;

int main()
{
    Expectation expect;
    
    cout << "Hello" << endl;

    cout << "Average Earning" << expect.CalculateExpectation(5) << endl;
}