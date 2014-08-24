#!/usr/bin/env ruby
# -*- coding: utf-8-unix -*-

abort "Ruby version is too old (1.9 or later is required)." if RUBY_VERSION < "1.9"

def escape_str_for_interpolate_fn_gen (str)
  str.gsub(/"/ ){'\\"'}
end

def escape_ruby_str_for_emacs! (str)
  str.gsub!(/\\/) {'\\\\'}
  str.gsub!(/"/ ) {'\\"'}
end

def process_replace (__str_body__, __str_regx__, __str_rpla__,
                     __dot_p__, __case_p__, __ext_p__, __eval_p__,
                     __limit__, __pos_start__, __rgn_beg__, __rgn_end__)
  __pos_wrap_end__ = nil
  __count__        = 0
  
  __regx__ = Regexp.new(__str_regx__, ((__dot_p__  ? Regexp::MULTILINE : 0)  |
                                       (__case_p__ ? 0 : Regexp::IGNORECASE) |
                                       (__ext_p__  ? Regexp::EXTENDED  : 0)))
  __interpolate_fn__ = begin
                         (__eval_p__ ?
                          eval('Proc.new '+__str_rpla__) :
                          eval('Proc.new {"'+escape_str_for_interpolate_fn_gen(__str_rpla__)+'"}'))
                       rescue SyntaxError, ArgumentError
                         $stderr.print "Syntax error in replacement \"#{__str_rpla__}\".\n"
                         $stderr.print $!.message
                         exit! 1
                       end
  
  __replace_fn__ = Proc.new { |__rgn_beg__, __rgn_end__, __wrap_p__|
    __pos__ = __rgn_beg__
    __last_0_width_pos__ = nil
    
    while ((__limit__ ? (__count__ < __limit__) : true)&& __str_body__.match(__regx__, __pos__)) do
      m = Regexp.last_match
      
      __match_beg__ = m.begin(0)
      __match_end__ = m.end(0)
      __0_width_p__ = (__match_beg__ == __match_end__)
      
      break if (__match_end__ > __rgn_end__)
      break if (__wrap_p__ && __pos_wrap_end__ && (__pos_wrap_end__ <= __match_beg__))
      __pos_wrap_end__ = __match_beg__ if ((not __wrap_p__) && (not __pos_wrap_end__))
      
      if (__0_width_p__ && __last_0_width_pos__ && (__match_beg__ == __last_0_width_pos__)) then
        # Do not enter into endless loop.
        __pos__ += 1
        break if (__pos__ > __rgn_end__)
        next
      elsif __0_width_p__ then
        __last_0_width_pos__ = __match_beg__
      else
        __last_0_width_pos__ = nil
      end
      
      __replacement__ = begin
                          __interpolate_fn__.call(m[0]).to_s
                        rescue Exception
                          $stderr.print "Error while evaluating replacement \"#{__str_rpla__}\".\n"
                          $stderr.print $!.message, "\n"
                          exit! 1
                        end
      
      escape_ruby_str_for_emacs!(__replacement__)
      
      print '(('
      m.length.times {|i|
        print m.begin(i), ' '
        print m.end(i),   ' '
      }
      print ')'
      print '"', __replacement__, '"'
      print ')'
      __count__ += 1
      __pos__   = __match_end__
    end
  }

  __rgn_beg__   = __rgn_beg__ || 0
  __rgn_end__   = __rgn_end__ || __str_body__.length
  __pos_start__ = ((__pos_start__ < __rgn_beg__) ?
                   __rgn_beg__ :
                   ((__pos_start__ > __rgn_end__) ?
                    __rgn_end__ :
                    __pos_start__))
  
  print "(setq result '("
  print "("
  __replace_fn__.call(__pos_start__, __rgn_end__, nil)
  print ")"
  
  print "("
  __replace_fn__.call(__rgn_beg__,
                      __pos_wrap_end__ ? __pos_wrap_end__ : __rgn_end__,
                      true)
  print ")"
  print "))\n"
  print ";;; EOF\n"
end

def main ()
  fn_body, fn_out, fn_regx, fn_rpla,
  dot_p, case_p, ext_p, eval_p,
  limit, pt_start, rgn_beg, rgn_end  = ARGV
  
  first_match_beg = nil

  str_body = open(fn_body, 'r:UTF-8') {|f| f.read}
  str_regx = open(fn_regx, 'r:UTF-8') {|f| f.read}
  str_rpla = open(fn_rpla, 'r:UTF-8') {|f| f.read}
  
  File.umask(0177)
  $stdout = open(fn_out, 'w:UTF-8')
  
  process_replace(str_body, str_regx, str_rpla, 
                  dot_p.empty?    ? nil : true,
                  case_p.empty?   ? nil : true,
                  ext_p.empty?    ? nil : true,
                  eval_p.empty?   ? nil : true,
                  limit.empty?    ? nil : limit.to_i,
                  pt_start.empty? ? nil : pt_start.to_i,
                  rgn_beg.empty?  ? nil : rgn_beg.to_i,
                  rgn_end.empty?  ? nil : rgn_end.to_i)
  
rescue Exception
  $stderr.print $!.message, "\n"
  exit! 1
end

main()

# EOF
