# Push to RC. 
# -r -- recursive
# -t -- preserve modification times
# -u -- update, skip files that are newer at the destination
# -v -- verbose
# -P -- provide a progress bar and allow resumption of interrupted transfers
# -z -- compress the transfer
# --exclude=".*" -- exclude hidden files
# --exclude=".*/" -- exclude hidden directories

rsync -rtuvPz --exclude=".*" --exclude ".*/" data/ dabr5651@login.rc.colorado.edu:/work/KellerLab/david/cotwins-analyses/data

# Pull from RC

rsync -rtuvPz --exclude=".*" --exclude ".*/" dabr5651@login.rc.colorado.edu:/work/KellerLab/david/cotwins-analyses/data/ data

