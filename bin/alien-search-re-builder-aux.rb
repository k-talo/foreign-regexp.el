#!/usr/bin/env ruby
# -*- coding: utf-8-unix -*-

abort "Ruby version is too old (1.9 or later is required)." if RUBY_VERSION < "1.9"

def main ()
  fn_in, fn_out, fn_pat = ARGV
  
  str_in  = open(fn_in,  'r:UTF-8') {|f| f.read}
  str_pat = open(fn_pat, 'r:UTF-8') {|f| f.read}
  
  $stdout = open(fn_out, 'w:UTF-8')
  
  print "(setq result '("
  
  str_in.scan( Regexp.new(str_pat) ) do
    print '('
    Regexp.last_match.length.times {|i|
      print Regexp.last_match.begin(i), ' '
      print Regexp.last_match.end(i),   ' '
    }
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
