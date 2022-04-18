#!/bin/bash
for ((i = 0; i < 20; i++))
do
  ./client -s domates -o data/data.csv & 
  sleep 1
  ./client -s domates -o data/data2.csv &
  sleep 1
done