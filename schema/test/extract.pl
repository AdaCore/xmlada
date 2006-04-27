#!/usr/bin/perl

$test=$ARGV[0];

open (FILE, "xmltests/resultNIST_$test.htm") || die "No such file; xmltests/resultNIST_$test.htm";
while (<FILE>) {
   if (m,href="./nisttest/NISTTestsAll/([^"]+)".*href="./nisttest/NISTTestsAll/([^"]+)",) {
      print "   expect \"\$nist/$1\" \"\$nist/$2\" \"\";\n";
   }
}
close (FILE);
