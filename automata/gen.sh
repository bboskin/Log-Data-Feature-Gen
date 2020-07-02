
# to run this file, enter: sudo sh ./gen.sh
# to the command line from the automata subdirectory.
# The first time you use it in a Termial session (or when you edit this file)
# you'll have to enter your computer's password




echo "Generating and applying all features from current micro grammar..."

# racket nasa.rkt -1 ../input-automata-csv/nasa-20K-row.csv > ../output-automata-csv/latest-data/micro.csv
#racket covid.rkt -1 ../covid-input-automata-csv/covid-counties-55k-input.csv > ../output-automata-csv/latest-data/micro.csv

echo "All done with micro Grammar!"

echo "Generating and applying all features from current finite grammar..."

#racket covid.rkt 0 ../covid-input-automata-csv/covid-counties-55k-input.csv > ../output-automata-csv/latest-data/finite.csv

# racket nasa.rkt 0 ../input-automata-csv/nasa-20K-row.csv > ../output-automata-csv/latest-data/finite.csv

echo "All done with finite Grammar!"

echo "Generating and applying all requested features from current infinite grammar..."

racket covid.rkt 3 ../covid-input-automata-csv/covid-counties-55k-input.csv > ../output-automata-csv/latest-data/5k.csv

# racket nasa.rkt 5 ../input-automata-csv/nasa-20K-row.csv > ../output-automata-csv/latest-data/5k-features.csv

echo "All done with infinite Grammar!"
