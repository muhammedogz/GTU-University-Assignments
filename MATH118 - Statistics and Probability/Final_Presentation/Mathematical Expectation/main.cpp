#include <iostream>
#include "Expectation.hpp"

using namespace std;

void exampleDiceTest(Expectation& expect);
void exampleAppointmentTest(Expectation& expect);

int main()
{
    Expectation expect;

    // exampleDiceTest(expect);
    exampleAppointmentTest(expect);
    
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