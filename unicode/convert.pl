#!/usr/bin/env perl

$basedir="/home/briot/perl-5.7.0/lib/unicode";

$blocks="$basedir/Blocks.txt";
$names="$basedir/Names.txt";

$name_index = 0;
$current = 0;
%list = ();
    



sub to_ada() {
    my($original_name) = shift;
    my ($name) = $original_name;

    $name =~ s/\([^\)]+\)//g;
    $name =~ s/\*//g;
    $name =~ s/^\s+//;
    $name =~ s/\s+$//;

    $name =~ s/\s+/_/g;
    $name =~ s/[-\'.]/_/g;
    $name =~ s/__+/_/g;
    $name =~ s/_$//;

    @letters = split (/ */, $name);
    $name = "";
    $to_upper = 1;
    foreach (@letters) {
	if ($to_upper) {
	    $name = $name . uc ($_);
	} else {
	    $name = $name . lc ($_);
	}

	$to_upper = ($_ eq '_');
    }

    $real_name = "";
    do {$real_name = $original_name, $name = "Nul"} if ($name eq "Null");
    do {$real_name = $original_name, $name = "Unicode_End"} if ($name eq "End");
    do {$real_name = $original_name, $name = "Unicode_Xor"}
        if ($name eq "Xor");
    do {$real_name = $original_name, $name = "Unicode_Not"}
        if ($name eq "Not");
    do {$real_name = $original_name, $name =~ s/[!]//g;} if ($name =~ /!/); 
   
    return $name;
}

sub print_line() {
    my ($current) = shift;
    my ($char_name) = &to_ada (shift);
    my ($out_line);

    if (! defined $list {$char_name}) {
	$list {$char_name} ++;
    
	$out_line = "   " . (sprintf ("%-39s", $char_name))
	    . " : constant Unicode_Char := 16#$current#;\n";
	
	if ($real_name ne "") {
	    print OUT "   --  Real Unicode name is $real_name\n";
	}
	if (length ($out_line) > 80) {
	    print OUT (join (":\n      ", split (/: /, $out_line)));
	} else {
	    print OUT "$out_line";
	}
    } else {
	print OUT "   --  Duplicate entry for $char_name\n";
    }
}


open (BLOCKS, "$blocks");
open (NAMES, "$names");
# @names=<NAMES>;
# close (NAMES);

do {
    $line = <NAMES>;
    chop ($line);
} while ($line !~ /^[0-9A-F]/);


while (<BLOCKS>) {
    # Skip comments
    next if ($_ =~ /^\#/);

    ($cat_start, $cat_end, $name) = split (/; /);

    $name = &to_ada ($name);

    $pkg = $name;
    $pkg =~ tr/A-Z/a-z/;

    next if ($pkg eq "");
    
    open (OUT, ">unicode-names-$pkg.ads");
    print OUT "--  This file is built automatically from data found on the\n";
    print OUT "--  unicode web site (http://www.unicode.org)\n\n";
    print OUT "package Unicode.Names.$name is\n";

    %list = ();
    
    while (1) {
	($current, $char_name) = ($line =~ /^([0-9A-F]+)\s+(.*)/);
	last if (($current cmp $cat_end) >= 0);
	
	if ($char_name !~ /<control>|<reserved>/) {
	    &print_line ($current, $char_name);
	}
	
	do {
	    $line = <NAMES>;
	    chop ($line);
	    if ($line =~ /^\s+=\s+(.*)/) {

		# In case there is a comma inside parenthesis
		$char_name = $1;
		$char_name =~ s/\([^\)]+\)//g;
		
		foreach $char_name (split (/,/, $char_name)) {
		    &print_line ($current, $char_name);
		}
	    }
	} until ($line =~ /^[0-9A-F]/);
    }

    print OUT "end Unicode.Names.$name;\n";
}

    
    
