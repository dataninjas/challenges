# Random line selection adapted from comments at http://stackoverflow.com/questions/10819911/read-random-lines-from-huge-csv-file-in-python
# Count file lines adapted from comments at http://stackoverflow.com/questions/845058/how-to-get-line-count-cheaply-in-python

import random
import csv

def file_len(fname):
  with open(fname) as f:
    for i, l in enumerate(f):
      if (i+1)%1000000 == 0:
        print("Read", i+1, "lines so far")
        pass
    print("The file has", i+1, "lines")
    return i + 1
  
def generate_sample(fname, fsize, count_desired, outfname, has_header=True):
  result = []
  
  print(fname)
  print(fsize)
  print(count_desired)
  print(outfname)

  p_select = count_desired / fsize
  print(p_select)
  count_added = 0
  for i, l in enumerate(open(fname)):
    if i == 0 and has_header:
      continue
    elif random.random() < p_select:
      count_added += 1
      print("Added", count_added, "lines total ( target =", count_desired,")")
      result.append(l)
  outfile=open(outfname , 'w')    
  outfile.write("".join(result))