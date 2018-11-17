#! /bin/bash

DATA_DIR="/gpfs/projects/gavingrp/dongmeic/daymet/evapo_na/input"

for YEAR in `ls $DATA_DIR`; do
    for FILE_NAME in `ls $DATA_DIR/$YEAR`; do
        python3 splash_main.py $DATA_DIR/$YEAR/$FILE_NAME&
    done
done
            
