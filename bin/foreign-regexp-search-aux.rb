#!/usr/bin/env ruby
# -*- coding: utf-8-unix -*-

abort "Ruby version is too old (1.9 or later is required)." if RUBY_VERSION < "1.9"

def main ()
  fn_in, fn_out, fn_pat, dot_p, case_p, ext_p, limit = ARGV
  
  str_in  = open(fn_in,  'r:UTF-8') {|f| f.read}
  str_pat = open(fn_pat, 'r:UTF-8') {|f| f.read}
  
  pat = Regexp.new(str_pat, ((dot_p.empty?  ? 0 : Regexp::MULTILINE)  |
                             (case_p.empty? ? Regexp::IGNORECASE : 0) |
                             (ext_p.empty?  ? 0 : Regexp::EXTENDED)))
  
  $stdout = open(fn_out, 'w:UTF-8')
  
  print "(setq result '("
  
  limit = (Integer limit rescue nil)
  count = 0
  
  str_in.scan( pat ) do
    break unless (!limit || ((count += 1) <= limit))
    
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
