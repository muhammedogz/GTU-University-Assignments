#ifndef CLIENT_H
#define CLIENT_H

int detectArguments(int argc, char *argv[], char **pathToServerFifo, char **pathToDataFile);

void invalidUsage();

#endif