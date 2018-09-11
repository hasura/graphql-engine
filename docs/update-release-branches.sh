#!/bin/bash

git checkout stg-release-1.0
git merge master
git push
git checkout release-1.0
git merge master
git push
git checkout master
