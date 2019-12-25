#!/usr/bin/perl

package t::inheritance;

use parent -norequire => ('t::Foo', 't::Bar');

__PACKAGE__->new()->runtests() if !caller;

#----------------------------------------------------------------------

package t::Foo;

use Test2::V0;

use parent qw( Test::Class::Tiny );

sub T1_test1 {
    ok 1;
}

#----------------------------------------------------------------------

package t::Bar;

use Test2::V0;

use parent qw( Test::Class::Tiny );

sub T1_test2 {
    ok 1;
}

1;
