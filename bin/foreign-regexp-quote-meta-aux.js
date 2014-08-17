#!/usr/bin/env node
// -*- coding: utf-8-unix -*-

const ENCODING = 'utf8';
const FS       = require('fs');
const UTIL     = require('util');

function write (path, str) {
    if (path == '/dev/stdout') UTIL.print(str)
    else FS.appendFileSync(path, str, ENCODING);
}

function escape_js_str_for_emacs (txt) {
    var retval = String(txt);
    retval = retval.replace(/\\/g, "\\\\");
    retval = retval.replace(/"/g, "\\\"");
    return retval;
}

function quotemeta (txt) {
    return txt.replace(/[.|()[\]{}+\\^$*?]/g, '\\$&');
}

function main (argv) {
    var fn_out   = argv[2];
    var fn_regx  = argv[3];
    
    var str_regx = FS.readFileSync(fn_regx, ENCODING);
    
    str_regx = quotemeta(str_regx);
    str_regx = escape_js_str_for_emacs(str_regx);
    
    write(fn_out, "(setq result \"");
    write(fn_out, str_regx);
    write(fn_out, "\")\n");
}


try {
    main(process.argv);
} catch (e) {
    console.error(e.message); // Do not show stack.
    process.exit(1);
}

process.exit(0);

// EOF
