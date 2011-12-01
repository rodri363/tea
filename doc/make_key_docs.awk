#!/usr/bin/awk -f 

# The goal: uniform model documentation for the keys, written in place in the code.
# 
# We'll start with something like
# /* \key db  The name of the databse.
# 
# and write out something like:
# 
# \key{db}{The name of the databse.}
#And then it's up to TeX to make that beautiful.

BEGIN { IGNORECASE=1 }

!/[ \t]*#/ {in_r_doc = 0}

(in_r_doc==1 || in_c_doc==1) && !/\\key/ {
        if ($0 ~ /^[ \t]*$/){ #complete blank ends the key item.
            in_r_doc=0
            in_c_cmt=0
        } else {
            if (sub("\\*\\/",""))
                in_c_doc=0
            sub("^[ \t]*#[' \t]*","", $0) #cut #' (if any).
            items[current_key] = items[current_key] "\n" $0 
        }
    }

/\/\*/ { in_c_cmt = 1 }  #does nothing right now

/\\key/ {
    if (in_c_cmt)
        in_c_doc = 1
    if ($0 ~ /[ \t]*#/){
        in_r_doc = 1
        in_c_doc = 0 #safety.
    }
    sub("^[ \t]*/\\*[ \t]*","", $0) #cut /* (if any).
    sub("^[ \t]*#[' \t]*","", $0) #cut # (if any).
    if ($0 ~ /key[ \t]*\{[^}]*\}/){
        current_key = $0
        sub("\\\\key[ \t]*\\{","", current_key) #cut \key, now that I know what it is.
        sub("\\}.*","", current_key) #cut \key, now that I know what it is.
        sub("\\\\key[ \t]*\\{[^}]*\\}","", $0) #cut \key, now that I know what it is.
    }
    else {
        sub("\\\\key[ \t]*","", $0) #cut \key, now that I know what it is.
        current_key = $1
        sub($1,"", $0)
    }
    gsub("_","\\_", current_key) #For tex. Should be obsolete.
    if ($0 ~ /\*\//)
        in_c_doc = 0
    sub("\\*\\/","", $0)
    items[current_key] = $0
    }

/\*\// { in_c_cmt = 0 
    in_c_doc = 0 }


END {
    cmd="sort > sortedkeys"
    for (k in items) print k | cmd
    close(cmd)
    i=0;
    while ((getline sitems[++i]<"sortedkeys") > 0)
        ;
    for (i in sitems){
        print "\\begin{key}{"sitems[i] "}"
        print items[sitems[i]] "\n\\end{key}\n"
    }
}
