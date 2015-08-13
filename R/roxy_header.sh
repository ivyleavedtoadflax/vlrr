for  file in *.R; do
cat roxy_header >> $file.temp
cat $file >> $file.temp
mv $file.temp $file
done
