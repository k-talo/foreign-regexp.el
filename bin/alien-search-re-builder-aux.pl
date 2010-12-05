#!/usr/bin/env perl
use strict;
use warnings;
use 5.008;

use Encode;
use utf8;

use English;
use FileHandle;

sub main () {
    my $fn_in     = shift @ARGV or die "No input  file name!";
    my $fn_out    = shift @ARGV or die "No output file name!";
    my $fn_pat    = shift @ARGV or die "No pattern file name!";
    my $code      = 'utf8';
    
    umask 0177;
    
    my($str_in, $str_pat, $str_repl);
    use PerlIO::encoding;
    local $PerlIO::encoding::fallback = Encode::FB_CROAK(); # Die on invalid char.
    {
        local $INPUT_RECORD_SEPARATOR = undef;
        $str_in   = FileHandle->new($fn_in,  "<:encoding($code)")->getline;
        $str_pat  = FileHandle->new($fn_pat, "<:encoding($code)")->getline;
    }
    
    {
        my $fh_out = FileHandle->new($fn_out, ">:encoding($code)");
        
        print $fh_out "(setq result '(";
        
        while ($str_in =~ m/${str_pat}/omg) {
            print $fh_out ' (';

            foreach my $i (0 .. $#LAST_MATCH_START) {
                print $fh_out $LAST_MATCH_START[$i], ' ';
                print $fh_out $LAST_MATCH_END  [$i], ' ';
            }
            print $fh_out ')',;
        }
        
        print $fh_out "))\n";
        print $fh_out ";;; EOF\n";
    }
    
    exit 0;
}

main();

# EOF
