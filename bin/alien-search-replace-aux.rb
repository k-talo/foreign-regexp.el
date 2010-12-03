#!/usr/bin/env ruby
# -*- coding: utf-8-unix -*-

abort "Ruby version is too old (1.9 or later is required)." if RUBY_VERSION < "1.9"

def escape_str_to_eval! (str)
  str.gsub!(/"/ ){'\\"'}
end

def escape_ruby_str_for_emacs! (str)
  str.gsub!(/\\/) {'\\\\'}
  str.gsub!(/"/ ) {'\\"'}
end

def main ()
  fn_in, fn_out, fn_pat, fn_rpl = ARGV
  
  str_in  = open(fn_in,  'r:UTF-8') {|f| f.read}
  str_pat = open(fn_pat, 'r:UTF-8') {|f| f.read}
  str_rpl = open(fn_rpl, 'r:UTF-8') {|f| f.read}
  
  escape_str_to_eval!(str_rpl)
  
  $stdout = open(fn_out, 'w:UTF-8')
  
  print "(setq result '("
  
  str_in.scan( Regexp.new(str_pat) ) do |m|
    replacement = eval '"' + str_rpl + '"'
    escape_ruby_str_for_emacs!(replacement)
    
    print '('
    print Regexp.last_match.begin(0), ' '
    print Regexp.last_match.end(0),   ' '
    print '"', replacement, '"'
    print ')'
  end
  
  print "))\n"
  print ";;; EOF\n"
  
  exit 0
  
rescue RegexpError
  $stderr.print $!.message
  exit 1
end

main

# EOF
