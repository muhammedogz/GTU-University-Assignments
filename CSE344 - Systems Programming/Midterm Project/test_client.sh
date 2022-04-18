#!/bin/bash
for ((i = 0; i < 20; i++))
do
  ./client -s serverFifo -o data/data.csv & 
  sleep 1
  ./client -s serverFifo -o data/data2.csv &
  sleep 1
done