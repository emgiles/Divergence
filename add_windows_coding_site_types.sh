###Quick command to add windows column to output.coding_site_types.tsv
##Copy paste below, change file names accordingly

awk 'BEGIN { FS = OFS = "\t" } 
     NR == 1 { print $0, "window"; next } 
     { 
         window = int(($2 - 1) / 20000) + 1; 
         print $0, window 
     }' your_file.tsv > output_file.tsv
