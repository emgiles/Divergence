#### Add midpoint for grouping coding site types in windows to compare to fst/dxy/fd results

awk 'BEGIN { FS = OFS = "\t" } 
     NR == 1 { print $0, "mid"; next } 
     { 
         if ($6 in mid) {
             print $0, mid[$6]
         } else {
             mid[$6] = int(($2 + ($6 - 1) * 20000 + ($6 * 20000 - 1)) / 2); 
             print $0, mid[$6]
         }
     }' your_file_windows.tsv > output_file_windows_mid.tsv
