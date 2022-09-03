#!/bin/sh
curl -H "Authorization: Bearer $ICFPC2022_API_TOKEN" -X POST -F file=@$2 https://robovinci.xyz/api/problems/$1
