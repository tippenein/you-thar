# YOU THAR?

# Goal

Scrape a large amount of blogs, returning only the ones active within a certain time period.

## Usage

```
csvtool col 2 blogs.csv | sort| uniq > justsites.txt
stack exec you-thar > they-are-there.csv
```

Currently, this will give you a csv with the site name and the time of their most recent posting.

