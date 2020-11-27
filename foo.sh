#!/bin/sh

LOG="/tmp/update.log"

r update.R >> ${LOG} 2>&1
r lageffect.R >> ${LOG} 2>&1
r regions.R >> ${LOG} 2>&1
DATE="2020-11-26"

cat README.template > README.md
echo "![GraphA](https://github.com/truthly/c19/blob/master/graphs/${DATE}a.png?raw=true \"GraphA\")" >> README.md
echo "![GraphB](https://github.com/truthly/c19/blob/master/graphs/${DATE}b.png?raw=true \"GraphB\")" >> README.md
echo "![GraphC](https://github.com/truthly/c19/blob/master/graphs/${DATE}c.png?raw=true \"GraphC\")" >> README.md

git add --all ~/c19 >> ${LOG} 2>&1
git commit -m "Add $DATE" >> ${LOG} 2>&1
git push >> ${LOG} 2>&1

