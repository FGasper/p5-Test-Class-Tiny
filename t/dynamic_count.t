#!/usr/bin/perl

package t::dynamic_count;

use Test2::V0;

use parent qw( Test::Class::Tiny );

__PACKAGE__->new()->runtests() if !caller;

sub runtests {
    my ($self) = @_;

    $self->num_method_tests( 'dynamic', 2 );

    return $self->SUPER::runtests();
}

sub dynamic {
    ok(1, "pass");
    ok(23, 'extra');
}

1;
