#!/usr/bin/env python

import sys, re, codecs


def interpolate_fn_gn (txt):
    env = {}
    txt = escape_str_for_interpolate_fn_gen(txt)
    
    m_lambda = re.match(r'^lambda', txt)
    m_def = re.search(r'^def\s+([^\s]+)\s*\(', txt, re.MULTILINE)
    
    if (m_lambda):
        txt = "interpolate_fn = " + txt
    elif (m_def):
        txt = (txt + "\ninterpolate_fn = " + m_def.group(1) + "\n")
    else:
        sys.stderr.write('Replacement is not type of lambda or def.')
        exit(1)
    
    try:
        exec(txt, {}, env)
    except Exception as e:
        sys.stderr.write("Error in replacement string: ")
        sys.stderr.write(str(e))
        exit(1)
    
    return env['interpolate_fn']


def escape_str_for_interpolate_fn_gen (txt):
  txt = re.sub(r'"', r'\"', txt)
  return txt


def escape_python_str_for_emacs (txt):
    txt = re.sub(r'\\', r'\\\\', txt)
    txt = re.sub(r'"', r'\\"', txt)
    return txt


pos_wrap_end = None
count_match = 0

def do_replace (f, str_body, regx, str_rpla, interpolate_fn, limit, rgn_beg, rgn_end, wrap_p):
    global pos_wrap_end
    global count_match
    
    for match in regx.finditer(str_body, rgn_beg):
        if (limit != None) and (limit <= count_match):
            break
        
        if match.end() > rgn_end:
            break
        
        if wrap_p and (pos_wrap_end != None) and (pos_wrap_end <= match.start()):
            break
        
        if (not wrap_p) and (pos_wrap_end == None):
            pos_wrap_end = match.start()
        
        f.write("((")
        f.write("%d %d " % match.span())
        for i in range(len(match.groups())):
            if (match.groups())[i] != None: # Skip unmatched group.
                f.write("%d %d " % match.span(i+1))
        f.write(")\"")
        
        try:
            rpla_expanded = (interpolate_fn and
                             [interpolate_fn(match)] or
                             [match.expand(str_rpla)])[0]
            if not (isinstance(rpla_expanded, unicode) or isinstance(rpla_expanded, str)):
                rpla_expanded = str(rpla_expanded)
            
            f.write(escape_python_str_for_emacs(rpla_expanded))
        except Exception as e:
            sys.stderr.write("Error while interpolating replacement: ")
            sys.stderr.write(str(e))
            exit(1)
        
        f.write("\")")
        
        count_match += 1


def main ():
    [prog, fn_body, fn_out, fn_regx, fn_rpla,
     dot_p, case_p, ext_p, eval_p,
     limit, pos_start, rgn_beg, rgn_end] = sys.argv
    
    limit     = (len(limit)     and [int(limit)]     or [None])[0]
    pos_start = (len(pos_start) and [int(pos_start)] or [None])[0]
    rgn_beg   = (len(rgn_beg)   and [int(rgn_beg)]   or [None])[0]
    rgn_end   = (len(rgn_end)   and [int(rgn_end)]   or [None])[0]
    
    str_body = codecs.open(fn_body, 'r', 'utf_8').read()
    str_regx = codecs.open(fn_regx, 'r', 'utf_8').read()
    str_rpla = (fn_rpla and [codecs.open(fn_rpla, 'r', 'utf_8').read()] or [""])[0]
    
    f = codecs.open(fn_out, 'w', 'utf8')
    
    
    try:
        regx = re.compile(str_regx, ((dot_p        and re.DOTALL     or 0) |
                                     ((not case_p) and re.IGNORECASE or 0) |
                                     (ext_p        and re.VERBOSE    or 0) |
                                     re.MULTILINE)) # XXX: Put re.UNICODE flag?
    except Exception as e:
        sys.stderr.write("Error while compiling regexp: ")
        sys.stderr.write(str(e))
        exit(1)
        
    interpolate_fn = (eval_p and [interpolate_fn_gn(str_rpla)] or [None])[0];
    
    rgn_beg   = rgn_beg or 0
    rgn_end   = rgn_end or len(str_body)
    pos_start = ((pos_start < rgn_beg) and
                 [rgn_beg] or
                 [((pos_start > rgn_end) and
                   [rgn_end] or
                   [pos_start])[0]])[0]
    
    f.write("(setq result '((")
    
    do_replace(f, str_body, regx, str_rpla, interpolate_fn, limit, pos_start, rgn_end, False)
    
    f.write(")(")
    
    do_replace(f, str_body, regx, str_rpla, interpolate_fn, limit, rgn_beg,
               ((pos_wrap_end != None) and
                [pos_wrap_end] or
                [rgn_end])[0],
               True)
    
    f.write(")))\n;;; EOF\n")
    
    exit

main()

#EOF
