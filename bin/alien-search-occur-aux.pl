#!/usr/bin/env perl
use strict;
use warnings;
use 5.008;

use Encode;
use utf8;

use English qw( -no_match_vars );
use FileHandle;

sub main () {
    my $fn_in     = shift @ARGV or die "No input  file name!";
    my $fn_out    = shift @ARGV or die "No output file name!";
    my $fn_pat    = shift @ARGV or die "No pattern file name!";
    my $dot_p     = @ARGV ? shift(@ARGV) : die "No dot matches new line flag.";
    my $case_p    = @ARGV ? shift(@ARGV) : die "No case sensitive flag.";
    my $ext_p     = @ARGV ? shift(@ARGV) : die "No extended regular expression flag.";;
    my $code      = 'utf8';
    my $offset    = 0;
    
    umask 0177;
    
    my($str_in, $str_pat, $str_repl);
    use PerlIO::encoding;
    local $PerlIO::encoding::fallback = Encode::FB_CROAK(); # Die on invalid char.
    {
        local $INPUT_RECORD_SEPARATOR = undef;
        $str_pat  = FileHandle->new($fn_pat, "<:encoding($code)")->getline;
    }
    
    my $pat = eval("qr/\${str_pat}/om" .
                   ( $dot_p  ? "s" : "") .
                   (!$case_p ? "i" : "") .
                   ( $ext_p  ? "x" : ""));
    die $EVAL_ERROR if $EVAL_ERROR;
    
    {
        local $INPUT_RECORD_SEPARATOR = "\n";
        my $fh_in  = FileHandle->new($fn_in, "<:encoding($code)");
        my $fh_out = FileHandle->new($fn_out, ">:encoding($code)");
        
        print $fh_out "(setq result '(\n";
        
        while (my $line = <$fh_in>) {
            my $len     = length $line;
            my $matched = 0;
            chomp $line;
            
            
            while ($line =~ m/${pat}/g) {
                print $fh_out '(' unless $matched++;
                print($fh_out
                      '(',
                      $offset + $LAST_MATCH_START[0], ' ',
                      $offset + $LAST_MATCH_END  [0],
                      ')');
            }
            
            print $fh_out ')' if $matched;
                
            $offset += $len;
        }
        
        print $fh_out "))\n";
        print $fh_out ";;; EOF\n";
    }
    
    exit 0;
}

main();

# EOF
