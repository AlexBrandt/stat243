export PATH=$PATH:/root/ephemeral-hdfs/bin/

hadoop fs -mkdir /data
hadoop fs -mkdir /data/airline

df -h

mkdir /mnt/airline
cd /mnt/airline

wget http://www.stat.berkeley.edu/share/paciorek/1987-2008.csvs.tgz

tar -xvf 1987-2008.csvs.tgz

hadoop fs -copyFromLocal /mnt/airline/*bz2 /data/airline

hadoop fs -ls /data/airline

yum install -y python27-pip python27-devel
pip-2.7 install 'numpy==1.9.2'

/root/spark-ec2/copy-dir /usr/local/lib64/python2.7/site-packages/numpy

export PATH=${PATH}:/root/spark/bin

# pyspark
