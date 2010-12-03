#!/usr/bin/env ruby
# -*- coding: utf-8-unix -*-

abort "Ruby version is too old (1.9 or later is required)." if RUBY_VERSION < "1.9"

def escape_ruby_str_for_emacs! (str)
  str.gsub!(/\\/) {'\\\\'}
  str.gsub!(/"/ ) {'\\"'}
end

def main ()
  fn_out, fn_pat = ARGV
  
  str_pat = open(fn_pat, 'r:UTF-8') {|f| f.read}
  
  $stdout = open(fn_out, 'w:UTF-8')
  
  retval = Regexp.escape(str_pat)
  escape_ruby_str_for_emacs!(retval)
  
  print '(setq result "'
  print retval
  print '")'

  exit 0
end

main

# EOF
