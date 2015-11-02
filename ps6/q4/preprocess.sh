myyear=$1
# Extract the header so we can find our columns of interest
bzcat $myyear.csv.bz2 | head -n 1 > $myyear.header.txt
# We will use the file line coordinates as the proxy for index columns
sed -e $'s/,/\\\n/g' $myyear.header.txt > $myyear.header.nsv
# Our desired headers
for i in "UniqueCarrier" "Origin" "Dest" "Month" "DayOfWeek" "DepTime" "DepDelay"
do
    x=`grep -n ^$i$ $myyear.header.nsv | cut -d':' -f 1`
    v="$v $x"
done
echo $v
# Now $v contains our columns of interest, which we just need # to separate by commas to use with cut. A sed command will # accomplish this with ease.


bzcat $myyear.csv.bz2 | \
     cut -d, -f`echo $v | \
     sed 's/ /,/g'` | bzip2 > $myyear.pp.csv.bz2
