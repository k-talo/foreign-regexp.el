#!/usr/bin/env perl
use strict;
use warnings;
use 5.008;

use Encode;
use utf8;

package main;
use English qw( -no_match_vars );
use FileHandle;

sub generate_build_replacement_fn {
    # Eval replacement string in environment
    # which has no lexical variable.
    #
    # Special-variables in the replacement string
    # will be interpolated.
    eval 'sub {"'.$_[0].'"}';
}

sub eval_replacement {
    eval $_[0]
}

sub interpolate_replacement {
    eval '"'.$_[0].'"'
}

sub eval_and_interpolate_replacement {
    # Not tested yet.
    my $str_repl = shift;
    $str_repl = eval_replacement($str_repl);
    die "Error while evaluating replacement \"${str_repl}\":\n${EVAL_ERROR}" if $EVAL_ERROR;
    $str_repl = interpolate_replacement($str_repl);
    die "Error while interpolating replacement \"${str_repl}\":\n${EVAL_ERROR}" if $EVAL_ERROR;
    $str_repl
}

sub escape_str_to_eval {
    my $r_txt = shift;
    ${$r_txt} =~ s/\\/\\\\/og;
    ${$r_txt} =~ s/"/\\"/og;
}

sub escape_perl_str_for_emacs {
    my $r_txt = shift;
    ${$r_txt} =~ s/\\/\\\\/og;
    ${$r_txt} =~ s/"/\\"/og;
}

sub main () {
    my $fn_in     = shift @ARGV or die "No input  file name!";
    my $fn_out    = shift @ARGV or die "No output file name!";
    my $fn_pat    = shift @ARGV or die "No pattern file name!";
    my $fn_repl   = shift @ARGV or die "No replacement file name!";
    my $dot_p     = @ARGV ? shift(@ARGV) : die "No dot matches new line flag.";
    my $case_p    = @ARGV ? shift(@ARGV) : die "No case sensitive flag.";
    my $ext_p     = @ARGV ? shift(@ARGV) : die "No extended regular expression flag.";;
    my $code      = 'utf8';
    
    my($str_in, $str_pat, $str_repl);
    use PerlIO::encoding;
    local $PerlIO::encoding::fallback = Encode::FB_CROAK(); # Die on invalid char.
    {
        local $INPUT_RECORD_SEPARATOR = undef;
        $str_in   = FileHandle->new($fn_in,   "<:encoding($code)")->getline;
        $str_pat  = FileHandle->new($fn_pat,  "<:encoding($code)")->getline;
        $str_repl = FileHandle->new($fn_repl, "<:encoding($code)")->getline;
    }
    my $pat = eval("qr/\${str_pat}/om" .
                   ( $dot_p  ? "s" : "") .
                   (!$case_p ? "i" : "") .
                   ( $ext_p  ? "x" : ""));
    die $EVAL_ERROR if $EVAL_ERROR;
    
    escape_str_to_eval(\$str_repl);
    my $build_replacement_fn = generate_build_replacement_fn($str_repl);
    die "Error in replacement \"${str_repl}\":\n${EVAL_ERROR}" if $EVAL_ERROR;
    
    umask 0177;
    my $fh_out = FileHandle->new($fn_out, ">:encoding($code)");
    
    print $fh_out "(setq result '(", "\n";
    
    while ($str_in =~ m/${pat}/omg) {
        my $replacement = eval { $build_replacement_fn->() };
        die "Error while interpolating replacement \"${str_repl}\":\n${EVAL_ERROR}" if $EVAL_ERROR;
            
        escape_perl_str_for_emacs(\$replacement);
            
        print $fh_out " (";
        print $fh_out $LAST_MATCH_START[0], ' ';
        print $fh_out $LAST_MATCH_END  [0], ' ';
        print $fh_out '"', $replacement, '"';
        print $fh_out " )", "\n";
    }
    
    print $fh_out "))", "\n";
    print $fh_out ";;; EOF", "\n";

    exit 0;
}

main();

# EOF
