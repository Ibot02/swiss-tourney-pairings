#!/bin/bash

rm -r player round static
wget -r --convert-links --adjust-extension localhost:3002
mv 'localhost:3002'/* .
rmdir 'localhost:3002'
