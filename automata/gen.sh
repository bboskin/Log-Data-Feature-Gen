
# to run this file, enter: sudo sh ./gen.sh
# to the command line. The first time you use it in a Termial sessions
# (or when you edit this file) you'll have to enter your computer's password


echo "Generating and applying all features from current micro grammar..."

racket nasa.rkt -1 ../input-automata-csv/5k-with-url.csv > ../output-automata-csv/latest-data/micro.csv

echo "All done with micro Grammar!"

echo "Generating and applying all features from current finite grammar..."

racket nasa.rkt 0 ../input-automata-csv/5k-with-url.csv > ../output-automata-csv/latest-data/finite.csv

echo "All done with finite Grammar!"

echo "Generating and applying all requested features from current infinite grammar..."

racket nasa.rkt 5 ../input-automata-csv/5k-with-url.csv > ../output-automata-csv/latest-data/5k-features.csv

echo "All done with infinite Grammar!"
