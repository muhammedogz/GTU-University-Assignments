#!/bin/bash
for ((i = 0; i < 20; i++))
do
  ./client -s serverFifo -o data/data.csv & 
  sleep .9
  ./client -s serverFifo -o data/data2.csv &
  sleep .9
done