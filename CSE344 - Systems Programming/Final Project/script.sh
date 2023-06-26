#!/bin/bash

PORT=33000

./server -p $PORT -t 11 &
sleep 1
./servant -d data/dataset -c 1-9 -r 127.0.0.1 -p $PORT &
./servant -d data/dataset -c 10-18 -r 127.0.0.1 -p $PORT &
./servant -d data/dataset -c 19-27 -r 127.0.0.1 -p $PORT &
./servant -d data/dataset -c 28-36 -r 127.0.0.1 -p $PORT &
./servant -d data/dataset -c 37-45 -r 127.0.0.1 -p $PORT &
./servant -d data/dataset -c 46-54 -r 127.0.0.1 -p $PORT &
./servant -d data/dataset -c 55-63 -r 127.0.0.1 -p $PORT &
./servant -d data/dataset -c 64-72 -r 127.0.0.1 -p $PORT &
./servant -d data/dataset -c 73-81 -r 127.0.0.1 -p $PORT &
sleep 3
./client -r data/requestFile -q $PORT -s 127.0.0.1


