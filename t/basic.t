#!/usr/bin/perl

package t::basic;

use Test2::V0;

use Test::Deep ();

use parent qw( Test::Class::Tiny );

__PACKAGE__->new()->runtests() if !caller;

sub T1_test_ok {
    ok(1, "pass");
    ok(23, 'extra');
}

sub T1_blows_up {
    ok(1, "pass");
    die 123123;
}

sub T0_not_counted {
    Test::Deep::cmp_deeply(
        [ 1, 2, 3 ],
        [ 1, 2, 3 ],
        'deep, old stuff',
    );
}

1;
