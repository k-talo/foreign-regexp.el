#!/usr/bin/env python

import sys, re, codecs

def escape_python_str_for_emacs (txt):
    txt = re.sub(r'\\', r'\\\\', txt)
    txt = re.sub(r'"', r'\\"', txt)
    return txt

def main ():
    [prog, fn_out, fn_regx] = sys.argv
    
    str_regx  = codecs.open(fn_regx, 'r', 'utf_8').read()
    
    f = codecs.open(fn_out, 'w', 'utf8')
    
    str_regx = re.escape(str_regx);
    str_regx = escape_python_str_for_emacs(str_regx);
    
    f.write("(setq result \"")
    f.write(str_regx)
    f.write("\")\n;;; EOF\n")
    
    exit(0)

main()

# EOF
