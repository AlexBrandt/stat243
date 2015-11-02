import time
from operator import add
import numpy as np
from pyspark import SparkContext

sc = SparkContext()

print "Read in start/stop times..."
print time.strftime("%H:%M:%S")
lines = sc.textFile('/data/airline')
print time.strftime("%H:%M:%S")
# particularly for in-class demo - good to repartition the 3 files to more p
lines = lines.repartition(96).cache()

numLines = lines.count()

# mapper
def computeKeyValue(line):
    vals = line.split(',')
    if vals[4] is not 'NA':
        vals[4] = str(vals[4]).zfill(4)[0:2]
    time_delay = vals[15]

    return("-".join([str(vals[8]), str(vals[16]), str(vals[17]), str(vals[1]),str(vals[3]), str(vals[4])]),[vals[15]])

def medianFun(input):
    c30 = 0
    c60 = 0
    c120 = 0
    t = 0
    for i in input[1]:
        if int(i) > 120:
            c120 += 1
        if int(i) > 60:
            c60 += 1
        if int(i) > 30:
            c30 += 1
        t += 1
    return((input[0],( c30, c60, c120, t)))

lines = lines.filter(lambda line: line.split(',')[15] != "NA" and line.split(',')[15] != "DepDelay")

def printable(input):
    vals = input[0].split('-')
    c30  = input[1][0]
    c60  = input[1][1]
    c120 = input[1][2]
    t    = input[1][3]

    return "%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%f" % (str(vals[0]), str(vals[1]), 
    str(vals[2]), str(vals[3]), str(vals[4]), 
    str(vals[5]), str(c30), str(c60), str(c120), str(t), float(c30)/float(t))
print "Query start/stop times..."
print time.strftime("%H:%M:%S")
output = lines.map(computeKeyValue).reduceByKey(add)
print time.strftime("%H:%M:%S")


# print output.collect()[0:10]

print "Delay binning start/stop times..."
print time.strftime("%H:%M:%S")
myResults = output.map(medianFun)
print myResults.take(1)
print time.strftime("%H:%M:%S")
myResults.map(printable).repartition(1).saveAsTextFile('/data/airline_processed')
