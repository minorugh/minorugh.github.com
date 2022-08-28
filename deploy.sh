#!/bin/bash

hugo
git add . && git diff --cached --exit-code --quiet && echo "\nnothing to commit, working tree clean!"|| \
git commit -a -m "Updated: `date +'%Y-%m-%d %H:%M:%S'`" && \
git push origin main
