#!/bin/bash
for i in {0..100}
  do
    for j in {0..10}
      do
        ./client white
      done
    cp data.csv data.csv.bkup.$i
  done