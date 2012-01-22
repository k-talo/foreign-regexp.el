#!/usr/bin/env perl
use strict;
use warnings;
use 5.008;

use Encode;
use utf8;

use English qw( -no_match_vars );
use FileHandle;

sub escape_perl_str_for_emacs {
    my $r_txt = shift;
    ${$r_txt} =~ s/\\/\\\\/og;
    ${$r_txt} =~ s/"/\\"/og;
}

sub main () {
    my $fn_out    = shift @ARGV or die "No output file name!";
    my $fn_pat    = shift @ARGV or die "No pattern file name!";
    my $code      = 'utf8';
    
    umask 0177;
    
    my($str_pat);
    use PerlIO::encoding;
    local $PerlIO::encoding::fallback = Encode::FB_CROAK(); # Die on invalid char.
    {
        local $INPUT_RECORD_SEPARATOR = undef;
        $str_pat = FileHandle->new($fn_pat, "<:encoding($code)")->getline;
        $str_pat = quotemeta($str_pat);
        escape_perl_str_for_emacs(\$str_pat)
    }
    
    {
        my $fh_out = FileHandle->new($fn_out, ">:encoding($code)");
        
        print $fh_out "(setq result \"${str_pat}\")\n";
        print $fh_out ";;; EOF\n";
    }
    
    exit 0;
}

main();

# EOF
