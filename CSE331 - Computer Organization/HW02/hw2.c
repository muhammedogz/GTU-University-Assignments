#include <stdio.h>
#include <stdlib.h>

// maxSeqNum alogirthm
void maxSeq(int arr[], int size)
{
    // if size is smaller than 1, return -1
    if (size < 1)
        return;

    int *longestSeq = malloc(sizeof(int) * size);
    int longestSeqSize = 0;

    int tempSeq[size];

    for (int i = 0; i < size; i++)
    {
        int tempSeqSize = 0;
        int tempVal = arr[i];
        tempSeq[tempSeqSize++] = arr[i];
        for (int j = i; j < size; j++)
        {
            if (arr[j] > tempVal)
            {
                tempSeq[tempSeqSize++] = arr[j];
                tempVal = arr[j];
            }
        }

        if (longestSeqSize < tempSeqSize)
        {
            longestSeqSize = tempSeqSize;
            longestSeq = malloc(sizeof(int) * tempSeqSize);
            for (int k = 0; k < tempSeqSize; k++)
            {
                longestSeq[k] = tempSeq[k];
            }
        }
        printf("size: %d\n", tempSeqSize);

        for (int k = 0; k < tempSeqSize; k++)
            printf("%d ", tempSeq[k]);
        printf("\n");
    }

    printf("Longestsize %d:", longestSeqSize);
    for (int i = 0; i < longestSeqSize; i++)
    {
        printf("%d ", longestSeq[i]);
    }
    printf("\n");

    free(longestSeq);
}

// Program to implement atoi() in C
#include <stdio.h>

// A simple atoi() function
int myAtoi(char *str)
{
    // Initialize result
    int res = 0;

    // Iterate through all characters
    // of input string and update result
    // take ASCII character of corresponding digit and
    // subtract the code from '0' to get numerical
    // value and multiply res by 10 to shuffle
    // digits left to update running total
    for (int i = 0; str[i] != '\0'; ++i)
        res = res * 10 + str[i] - '0';

    // return result.
    return res;
}

int main()
{
    int arr[] = {15, 43, 28, 29, 35, 47};
    int n = sizeof(arr) / sizeof(arr[0]);
    maxSeq(arr, n);

    return 0;
}