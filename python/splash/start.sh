#! /bin/bash

DATA_DIR="./data/input" # <- Change this

for YEAR in `ls $DATA_DIR`; do
    for FILE_NAME in `ls $DATA_DIR/$YEAR`; do
        python3 splash_main.py $DATA_DIR/$YEAR/$FILE_NAME&
    done
done
            
