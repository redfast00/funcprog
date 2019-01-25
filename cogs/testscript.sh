#!/bin/bash
for file in tests/*
do
    echo "Now testing: $file"
    stack build --exec "cogs --test $file"
done

