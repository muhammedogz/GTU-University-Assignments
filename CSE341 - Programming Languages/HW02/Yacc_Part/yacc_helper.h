#ifndef __YACC_HELPER_H__
#define __YACC_HELPER_H__

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

// prevent simple yylex and yyerror warning. Not so impotant.
int yylex(); 
int yyerror(const char *error)
{
    printf("Syntax Error. Can not tokenize: '%s'\n",error);
    exit(1);
}

struct IdentifierStruct
{ 
    char identifier[1000][200]; 
    int values[1000]; 
    int count;
};

struct IdentifierStruct identifiers = {"", 0, 0};

int getIDentifierIndex(char *searchStr)
{
    for(int i = 0; i < identifiers.count; i++)
        if (strcmp(searchStr, identifiers.identifier[i]) == 0)
            return i;

    return -1;
}

int getIdentifierValue(char *searchStr)
{
    int index = getIDentifierIndex(searchStr);
    if (index == -1)
    {
        printf("Variable %s not defined!\n", searchStr);
        exit(0);
    }
    else
        return identifiers.values[index];
    
    return 0;
}

void addIdentifier(char *identifier, int value)
{
    int index = getIDentifierIndex(identifier);
    // if given identifier does not exist, add
    if(index == -1)
    {
        strcpy(identifiers.identifier[identifiers.count], identifier);
        identifiers.values[identifiers.count] = value;
        identifiers.count += 1;
    }
    // if already exist, update value
    else
        identifiers.values[index] = value;

}

// concatenate two int arrays and return first
int* concat_append(int *array1, int *array2, int sum, int size)
{
    int i = 0;
	for(int j = sum - size; j < sum; j++, i++)
	    array1[j] = array2[i];
    
    return array1;
}

// append the begining of the array
void appendBegining(int *arr, int size, int value)
{

    // shift all elements to the right
	for(int i = 0; i < size; i++)
	    arr[i+1] = arr[i];

	arr[0] = value;
}



// open file
int loadFile(char *fileName)
{
    printf("Loading file: %s\n", fileName);
    FILE *file = fopen(fileName, "r");
    if(file == NULL)
    {
        printf("Error: File %s not found.\n", fileName);
        return 0;
    }
    return 1;
}

void toUpper(char *str)
{
    for(int i = 0; i < strlen(str); i++)
        if (str[0] >= 'a' && str[0] <= 'z')
            str[0] = str[0] - 32;
}


void copy(int _v1[], int _v2[], int _size)
{
    
    for(int i = 0; i < _size && i < 999; i++)
        _v1[i] = _v2[i];
}


// print content due to type
void printSelector(void* param, int type, int size, int isList)
{
    printf("Result: ");
    // if integer (value) passed
    if (type == 0)
    {
        printf("%d", *(int*)param);
    }
    // if string (identifier) passed
    else if (type == 1)
    {
        char **temp = (char*)param;
        char *str = (char*)temp[0];
        while (*str != '\0')
        {
            printf("%c", *str);
            str++;
        }
    }
    else if (type == 2)
    {
        if (isList == 0)
        {
            if (*(int*)param == 1)
                printf("TRUE");
            else
                printf("FALSE");
        }
        else 
        {
            if (*(int*)param == 1)
                printf("T");
            else
                printf("NIL");
        }
        
    }
    // if array (list) passed
    else if (type == 3)
    {
        printf("(");
        for (int i = 0; i < size; i++)
        {
            printf("%d", *(int*)((char*)param + i * sizeof(int)));
            if (i != size - 1)
            {
                printf(" ");
            }
        }
        printf(")");
    }
    
    printf("\n");
}

void checkZeroDivision(int _value)
{
    if (_value == 0)
    {
        printf("Zero Division Error.\n");
        exit(1);
    }
}

int _pow(int _base, int _exp)
{
    int result = 1;
    for (int i = 0; i < _exp; i++)
        result *= _base;
    return result;
}


int var1 = 0;
int var2 = 0;
int sum = 0;
int isNil = 0;

#endif