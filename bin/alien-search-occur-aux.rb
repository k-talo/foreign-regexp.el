#!/usr/bin/env ruby
# -*- coding: utf-8-unix -*-

abort "Ruby version is too old (1.9 or later is required)." if RUBY_VERSION < "1.9"

def main ()
  fn_in, fn_out, fn_pat = ARGV
  
  str_pat = open(fn_pat, 'r:UTF-8') {|f| f.read}
  offset = 0
  
  $stdout = open(fn_out, 'w:UTF-8')
  
  print "(setq result '("
  
  open(fn_in, 'r:UTF-8') do |file_in|
    while line = file_in.gets do
      matched = 0
      len = line.length
      line.chomp!
      
      line.scan( Regexp.new(str_pat) ) do
        print '(' if matched == 0
        print '('
        print offset + Regexp.last_match.begin(0), ' '
        print offset + Regexp.last_match.end(0)
        print ')'
        matched += 1
      end
      print ')' if matched != 0
      
      offset += len
    end
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
