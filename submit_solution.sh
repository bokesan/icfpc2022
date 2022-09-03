#!/bin/sh
n=`basename $1 .png.txt`
echo "Submitting solition for $n"
curl -H "Authorization: Bearer $ICFPC2022_API_TOKEN" -X POST -F file=@$1 https://robovinci.xyz/api/problems/$n
