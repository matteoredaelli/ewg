#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin -DTEST -s ewg start
