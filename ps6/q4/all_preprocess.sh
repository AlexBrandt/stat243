date
for f in `seq 1987 1 2008`;
do
    echo $f
    ~/preprocess.sh $f
done
date
