#!/usr/bin/env ruby
# -*- coding: utf-8-unix -*-

abort "Ruby version is too old (1.9 or later is required)." if RUBY_VERSION < "1.9"

def escape_str_for_interpolate_fn_gen (str)
  str.gsub("\\"){"\\\\"}.gsub(/"/ ){'\\"'}
end

def escape_ruby_str_for_emacs! (str)
  str.gsub!(/\\/) {'\\\\'}
  str.gsub!(/"/ ) {'\\"'}
end

def process_replace (__str_in__, __pat__, __str_rpl__, __eval_p__)
   begin
    interpolate_fn = if __eval_p__
                     then eval 'Proc.new {'+__str_rpl__+'}'
                     else eval 'Proc.new {"'+escape_str_for_interpolate_fn_gen(__str_rpl__)+'"}' end
  rescue SyntaxError
    $stderr.print "Syntax error in replacement \"#{__str_rpl__}\".\n"
    $stderr.print $!.message
    exit 1
  end
  
  print "(setq result '("
  
  __str_in__.scan( __pat__ ) do |m|
    begin
      __replacement__ = interpolate_fn.call(m).to_s
      escape_ruby_str_for_emacs!(__replacement__)
    rescue Exception
      $stderr.print "Error while evaluating replacement \"#{__str_rpl__}\".\n"
      $stderr.print $!.message
      exit 1
    end
    
    print '('
    print Regexp.last_match.begin(0), ' '
    print Regexp.last_match.end(0),   ' '
    print '"', __replacement__, '"'
    print ')'
  end
  
  print "))\n"
  print ";;; EOF\n"
end

def main ()
  fn_in, fn_out, fn_pat, fn_rpl, dot_p, case_p, ext_p, eval_p = ARGV
  
  str_in  = open(fn_in,  'r:UTF-8') {|f| f.read}
  str_pat = open(fn_pat, 'r:UTF-8') {|f| f.read}
  str_rpl = open(fn_rpl, 'r:UTF-8') {|f| f.read}
  
  pat = Regexp.new(str_pat, ((dot_p.empty?  ? 0 : Regexp::MULTILINE)  |
                             (case_p.empty? ? Regexp::IGNORECASE : 0) |
                             (ext_p.empty?  ? 0 : Regexp::EXTENDED)))
  
  $stdout = open(fn_out, 'w:UTF-8')
  
  process_replace(str_in, pat, str_rpl, eval_p.empty? ? nil : true)
  
rescue Exception
  $stderr.print $!.message
  exit 1
end

main()

# EOF
