#!/bin/bash

for form in {1..27..1}
do
 echo $form
 bash start.sh "${form}" #change the path to where you store start.sh
 echo ' '
done
