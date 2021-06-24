#include <iostream>
#include <map>
#include "Expectation.hpp"

using namespace std;

void exampleDiceTest(Expectation& expect);
void exampleAppointmentTest(Expectation& expect);
void findVarianceTest(Expectation& expect);

int main()
{
    Expectation expect;

    cout << "**************** Dice Example ****************" << endl;
    exampleDiceTest(expect);
    cout << "**************** Appointment Example ****************" << endl;
    exampleAppointmentTest(expect);
    cout << "**************** Mean-Variance Example ****************" << endl;
    findVarianceTest(expect);
    
}

void findVarianceTest(Expectation& expect)
{
    int size = 0;
    cout << "Enter test size:";
    cin >> size;
    map<double, double> m;   
    cout << "Size:" << size << endl;
    for (int i = 0; i < size; i++)
    {
        double freq = 0;
        double f_x = 0;
        cout << "Enter " << i+1 << ". frequency and f(X) (probability) respectively" << endl;
        cin >> freq;
        cin >> f_x;
        cout << freq << " " << f_x << endl;
        m.insert(pair<double,double>(freq,f_x));
    }
    expect.varianceFinder(m, expect.meanFinder(m));

}

void exampleDiceTest(Expectation& expect) 
{
    cout << "Call diceExample() function with 1-10-100-1000-10000 respectively" << endl;
    cout << "Calling with 1 times" << endl; expect.exampleDice(1); cout << endl;
    cout << "Calling with 10 times" << endl; expect.exampleDice(10); cout << endl;
    cout << "Calling with 100 times" << endl; expect.exampleDice(100); cout << endl;
    cout << "Calling with 1000 times" << endl; expect.exampleDice(1000); cout << endl;
    cout << "Calling with 10000 times" << endl; expect.exampleDice(10000); cout << endl;
}

void exampleAppointmentTest(Expectation& expect) 
{
    cout << "Call diceExample() function with 1-10-100-1000-10000 respectively" << endl;
    cout << "Calling with 1 times" << endl; expect.exampleAppointment(1); cout << endl;
    cout << "Calling with 10 times" << endl; expect.exampleAppointment(10); cout << endl;
    cout << "Calling with 100 times" << endl; expect.exampleAppointment(100); cout << endl;
    cout << "Calling with 1000 times" << endl; expect.exampleAppointment(1000); cout << endl;
    cout << "Calling with 10000 times" << endl; expect.exampleAppointment(10000); cout << endl;
}