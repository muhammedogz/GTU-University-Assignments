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
    }

    printf("Longestsize %d:", longestSeqSize);
    for (int i = 0; i < longestSeqSize; i++)
    {
        printf("%d ", longestSeq[i]);
    }
    printf("\n");

    free(longestSeq);
}

int main()
{
    int arr[] = {1, 11, 10, 22, 9, 33, 21, 50, 41, 60};
    int n = sizeof(arr) / sizeof(arr[0]);
    maxSeq(arr, n);

    return 0;
}