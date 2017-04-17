#!/usr/bin/env bash

erl +P 2123123 -s -pa ./_build/default/lib/fl/ebin -s dining main 100000
