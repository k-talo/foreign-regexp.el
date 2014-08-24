#!/usr/local/bin/node --expose_natives_as=NATIVES
// -*- coding: utf-8-unix -*-

const ENCODING = 'utf8';
const FS       = require('fs');
const UTIL     = require('util');

// process.version

function write (path, str) {
    if (path == '/dev/stdout') UTIL.print(str)
    else FS.appendFileSync(path, str, ENCODING);
}

function eval_fn_gen (__expression__) {
    var indirect = eval; // For indirect eval call.
    return indirect("("+__expression__+")");
}

function quote_str_for_eval (str) {
    // str = str.replace(/\\/mg, '\\\\');
    str = str.replace(/"/mg, '\\"');
    return '"'+str+'"';
}

function escape_js_str_for_emacs (txt) {
    var retval = String(txt);
    retval = retval.replace(/\\/g, "\\\\");
    retval = retval.replace(/"/g, "\\\"");
    return retval;
}


function number_p (c) {
    var code = c.charCodeAt(0);
    return (0x30 <= code) && (code <= 0x39);
}

function interpolate_fn_gen (str, num_capture) {
    var cur_pos = 0;
    var last_match_end = 0;
    
    var rpla_lst = [];
    var pos;
    
    while (-1 < (pos = str.indexOf('$', last_match_end))) {
        rpla_lst.push(quote_str_for_eval(str.substring(last_match_end, pos)));
        
        var peek = str.substr(pos + 1, 1);
        var expr;

        if (peek == '$') {
            rpla_lst.push(quote_str_for_eval('$'));
            pos += 2;
        } else if (peek == "&") {
            expr = 'RegExp.lastMatch';
            rpla_lst.push('('+expr+'?'+expr+':"")');
            pos += 2;
        } else if (peek == "`") {
            expr = 'RegExp.leftContext';
            rpla_lst.push('('+expr+'?'+expr+':"")');
            pos += 2;
        } else if (peek == "'") {
            expr = 'RegExp.rightContext';
            rpla_lst.push('('+expr+'?'+expr+':"")');
            pos += 2;
        } else if (number_p(peek)
                   && peek != '0'
                   && number_p(str.substr(pos + 2, 1))
                   && (parseInt(str.substr(pos + 1, 2)) <= num_capture)) {
            expr = '$'+str.substr(pos + 1, 2);
            rpla_lst.push('('+expr+'?'+expr+':"")');
            pos += 3;
        } else if (number_p(peek)
                   && peek != '0'
                   && (parseInt(peek) <= num_capture)) {
            expr = '$'+peek;
            rpla_lst.push('('+expr+'?'+expr+':"")');
            pos += 2;
        } else {
            rpla_lst.push(quote_str_for_eval('$'));
            pos += 1;
        }
        last_match_end = pos;
    }
    rpla_lst.push(quote_str_for_eval(str.substring(last_match_end, str.length)));
    
    var arg_lst = ['_ignore_'];
    for (var i = 1; i <= num_capture; ++i) { arg_lst.push('$'+i) };
    
    return eval('(function ('+arg_lst.join(',')+'){return '+rpla_lst.join('+')+'})');
}

function process_replace (str_body, str_regx, str_rpla,
                          dot_p, case_p, ext_p, eval_p,
                          limit, pos_start, rgn_beg, rgn_end,
                          fn_out) {
    var match;
    var pos_wrap_end = null;
    var count        = 0;
    
    var regx = RegExp(str_regx,
                      "gm" +
                      (dot_p  ? "" : "")  + /* Not supported. Dot
                                               never matches a new
                                               line character. */
                      (case_p ? "" : "i") +
                      (ext_p  ? "" : "")); // Not supported

    var interpolate_fn = null;

    var replace_fn = function (rgn_beg, rgn_end, wrap_p) {
        var cur_pos = rgn_beg;
        last_0_width_pos = null;
        
        if (interpolate_fn == null) {
            regx.lastIndex = 0;
            if (match = regx.exec(str_body)) {
                num_capture = match.length - 1;
                
                try {
                    interpolate_fn = (eval_p
                                      ? eval_fn_gen(str_rpla)
                                      : interpolate_fn_gen(str_rpla, num_capture));
                } catch (e) {
                    throw {message: 'Syntax error in replacement "'+str_rpla+'":\n'+e.message};
                }
            }
        }

    
        while (((limit == null) ? true : (count < limit))
               && ((regx.lastIndex = cur_pos) ? true : true)
               && (match = regx.exec(str_body))) {
            
            var lmi = NATIVES.lastMatchInfo;
            
            // NOTE: lastMatchInfo seems to be broken when
            //       some I/O operations has been run.
            //       (I dunno why this happens...)
            //       So I save the original values of lastMatchInfo.
            var tmp = [];
            var lmi_len = lmi.length;
            for (var i = 0; i < lmi_len; ++i) {
                tmp[i] = lmi[i];
            }
            lmi = tmp;
            
            var num_capture = match.length - 1;
            var offset = 3; // from `macro CAPTURE(index)' in macros.py
            
            var match_beg = lmi[offset + 0];
            var match_end = lmi[offset + 1];
            var _0_width_p = (match_beg == match_end);
            
            if (match_end > rgn_end) { break };
            if (wrap_p && (pos_wrap_end != null) && (pos_wrap_end <= match_beg)) { break };
            if ((! wrap_p) && (pos_wrap_end == null))  { pos_wrap_end = match_beg };
            
            if (_0_width_p && (last_0_width_pos != null) && (match_beg == last_0_width_pos))
            {
                // Do not enter into endless loop.
                cur_pos += 1;
                if (cur_pos > rgn_end)  { break };
                continue;
                
            }  else if (_0_width_p) {
                last_0_width_pos = match_beg;
                
            } else {
                last_0_width_pos = null;
            }

            try {
                replacement = interpolate_fn.apply(this, match.concat([match_beg, str_body]));
            } catch (e) {
                throw {message: 'Error while interpolating replacement "'+str_rpla+'":\n'+e.message};
            }
            replacement = escape_js_str_for_emacs(replacement);
            
            write(fn_out, "((");
            
            for (var i = 0; i <= num_capture; ++i) {
                var idx = i*2 + offset;
                var sub_match_beg = lmi[idx];
                var sub_match_end = lmi[idx+1];
                
                if ((-1 < sub_match_beg) && (-1 < sub_match_end)) { // XXX: What this means?
                    write(fn_out, sub_match_beg + " " + sub_match_end + " ")
                }
            }
            write(fn_out, ")");
            write(fn_out, '"'+replacement+'"');
            write(fn_out, ")");
            
            cur_pos = match_end;
            ++count;
        }
    }
     
    rgn_beg   = (rgn_beg == null) ? 0: rgn_beg;
    rgn_end   = (rgn_end == null) ? str_body.length : rgn_end;
    pos_start = ((pos_start < rgn_beg)
                 ? rgn_beg
                 : ((rgn_end < pos_start)
                    ? rgn_end
                    : pos_start));
    
    write(fn_out, "(setq result '(")
    write(fn_out, "(");
    replace_fn(pos_start, rgn_end, false);
    write(fn_out, ")");

    write(fn_out, "(");
    replace_fn(rgn_beg, (pos_wrap_end == null) ? rgn_end : pos_wrap_end, true);
    write(fn_out, ")");
    
    write(fn_out, "))\n")
}

function main (argv) {
    var fn_body   = argv[2];
    var fn_out    = argv[3];
    var fn_regx   = argv[4];
    var fn_rpla   = argv[5];
    var dot_p     = argv[6].length? true : false;
    var case_p    = argv[7].length? true : false;
    var ext_p     = argv[8].length? true : false;
    var eval_p    = argv[9].length? true : false;
    var limit     = argv[10].length ? parseInt(argv[10]) : null;
    var pos_start = argv[11].length ? parseInt(argv[11]) : null;
    var rgn_beg   = argv[12].length ? parseInt(argv[12]) : null;
    var rgn_end   = argv[13].length ? parseInt(argv[13]) : null;
    
    var str_body = FS.readFileSync(fn_body,  ENCODING);
    var str_regx = FS.readFileSync(fn_regx, ENCODING);
    var str_rpla = FS.readFileSync(fn_rpla,  ENCODING);
    
    process_replace(str_body, str_regx, str_rpla, 
                    dot_p, case_p, ext_p, eval_p,
                    limit, pos_start, rgn_beg, rgn_end,
                    fn_out);
}


try {
    main(process.argv);
} catch (e) {
    console.error(e.message); // Do not show stack.
    process.exit(1);
}

process.exit(0);

// EOF
