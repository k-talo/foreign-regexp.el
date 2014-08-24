#!/usr/bin/env perl
# -*- coding: utf-8-unix -*-
use strict;
use warnings;
use 5.008;

use Encode;
use utf8;

package main;
use English qw( -no_match_vars );
use FileHandle;

sub interpolate_fn_gen {
    # Interpolate replacement string in environment
    # which has no lexical variable.
    #
    # Special-variables in the replacement string
    # will be interpolated.
    eval 'sub {"'. escape_str_for_interpolate_fn_gen(${$_[0]}) .'"}';
}

sub eval_fn_gen {
    # Eval replacement string in environment
    # which has no lexical variable.
    eval 'sub {'.${$_[0]}.'}';
}

sub escape_str_for_interpolate_fn_gen {
    my $txt = shift;
    $txt =~ s/"/\\"/og;
    $txt
}

sub escape_perl_str_for_emacs {
    my $r_txt = shift;
    ${$r_txt} =~ s/\\/\\\\/og;
    ${$r_txt} =~ s/"/\\"/og;
}

sub process_replace {
    my $r_str_body   = shift;
    my $r_str_regx   = shift;
    my $r_str_rpla   = shift;
    my $dot_p        = shift;
    my $case_p       = shift;
    my $ext_p        = shift;
    my $eval_p       = shift;
    my $limit        = shift;
    my $pos_start    = shift;
    my $rgn_beg      = shift;
    my $rgn_end      = shift;
    
    my $pos_wrap_end = undef;
    my $count        = 0;
    
    my $regx = eval ("qr/\${\$r_str_regx}/mo" .
                     ( $dot_p  ? "s" : "") .
                     (!$case_p ? "i" : "") .
                     ( $ext_p  ? "x" : ""));
    die $EVAL_ERROR if $EVAL_ERROR;
    
    my $interpolate_fn = ($eval_p
                          ? eval_fn_gen($r_str_rpla)
                          : interpolate_fn_gen($r_str_rpla));
    die "Syntax error in replacement \"${$r_str_rpla}\":\n${EVAL_ERROR}" if $EVAL_ERROR;
    
    my $replace_fn = sub {
        my $rgn_beg = shift;
        my $rgn_end = shift;
        my $wrap_p  = shift;
        
        
        pos(${$r_str_body}) = $rgn_beg;
        
        while (((defined $limit) ? ($count < $limit) : 1) && (${$r_str_body} =~ m/${regx}/g)) {
            my $match_beg = $LAST_MATCH_START[0];
            my $match_end = $LAST_MATCH_END  [0];
            
            last if ($match_end > $rgn_end);
            last if ($wrap_p && (defined $pos_wrap_end) && ($pos_wrap_end <= $match_beg));
            $pos_wrap_end = $match_beg if ((not $wrap_p) && (not (defined $pos_wrap_end)));
            
            my $replacement = eval { $interpolate_fn->() };
            die "Error while interpolating replacement \"${$r_str_rpla}\":\n${EVAL_ERROR}" if $EVAL_ERROR;
            
            escape_perl_str_for_emacs(\$replacement);
            
            print " ((";
            print $match_beg, ' ';
            print $match_end, ' ';
            foreach my $i (1 .. $#LAST_MATCH_START) {
                print $LAST_MATCH_START[$i], ' ';
                print $LAST_MATCH_END  [$i], ' ';
            }
            print " )";
            print '"', $replacement, '"';
            print " )", "\n";
            
            ++$count;
        }
    };
    
    $rgn_beg   = $rgn_beg || 0;
    $rgn_end   = $rgn_end || length(${$r_str_body});
    $pos_start = (($pos_start < $rgn_beg)
                  ? $rgn_beg
                  : (($pos_start > $rgn_end)
                     ? $rgn_end
                     : $pos_start));
    
    print "(setq result '(";
    print " (";
    $replace_fn->($pos_start, $rgn_end, 0);
    print " )";
    
    # Search wrap around.
    print " (";
    $replace_fn->($rgn_beg,
                  (defined $pos_wrap_end) ? $pos_wrap_end : $rgn_end,
                  1);
    print " )";
    print "))", "\n";
    print ";;; EOF", "\n";
}

sub main () {
    my $fn_body   = shift @ARGV or die "No input file name!";
    my $fn_out    = shift @ARGV or die "No output file name!";
    my $fn_regx   = shift @ARGV or die "No regexp file name!";
    my $fn_rpla   = @ARGV ? shift @ARGV : die "No replacement file!";
    my $dot_p     = @ARGV ? shift(@ARGV) : die "No dot matches new line flag.";
    my $case_p    = @ARGV ? shift(@ARGV) : die "No case sensitive flag.";
    my $ext_p     = @ARGV ? shift(@ARGV) : die "No extended regular expression flag.";
    my $eval_p    = @ARGV ? shift(@ARGV) : die "No eval replacement flag.";
    my $limit     = @ARGV ? shift(@ARGV) : die "No search limit.";
    my $pos_start = @ARGV ? shift(@ARGV) : die "No start position.";
    my $rgn_beg   = shift @ARGV;
    my $rgn_end   = shift @ARGV;
    
    my $code      = 'utf8';
    
    my($str_body, $str_regx, $str_rpla);
    
    use PerlIO::encoding;
    local $PerlIO::encoding::fallback = Encode::FB_CROAK(); # Die on invalid char.
    {
        local $INPUT_RECORD_SEPARATOR = undef;
        $str_body = FileHandle->new($fn_body, "<:encoding($code)")->getline;
        $str_regx = FileHandle->new($fn_regx, "<:encoding($code)")->getline;
        $str_rpla = $fn_rpla ? FileHandle->new($fn_rpla, "<:encoding($code)")->getline : "";
    }
    
    umask 0177;
    *STDOUT = FileHandle->new($fn_out, ">:encoding($code)");
    
    process_replace(\$str_body, \$str_regx, \$str_rpla,
                    $dot_p, $case_p, $ext_p, $eval_p,
                    length($limit) ? $limit : undef, $pos_start, $rgn_beg, $rgn_end);
    
    exit 0;
}

main();

# EOF
