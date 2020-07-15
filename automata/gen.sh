
# to run this file, enter: sudo sh ./gen.sh
# to the command line from the automata subdirectory.
# The first time you use it in a Termial session (or when you edit this file)
# you'll have to enter your computer's password




echo "Generating and applying all features from current micro grammar..."

# racket nyt.rkt -1 ../input-automata-csv/nyt-input-73K-row.csv > ../output-automata-csv/latest-data/micro.csv

echo "All done with micro Grammar!"

echo "Generating and applying all features from current finite grammar..."

# racket nyt.rkt 0 ../input-automata-csv/nyt-input-73K-row.csv > ../output-automata-csv/latest-data/finite.csv

echo "All done with finite Grammar!"

echo "Generating and applying all requested features from current infinite grammar..."

racket nyt.rkt 10 ../input-automata-csv/nyt-input-73K-row.csv > ../output-automata-csv/latest-data/10k.csv

echo "All done with infinite Grammar!"
