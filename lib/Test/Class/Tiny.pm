package Test::Class::Tiny;

use strict;
use warnings;

our $VERSION;
$VERSION = 0.01;

=encoding utf-8

=head1 NAME

Test::Class::Tiny - xUnit in Perl, simplified

=head1 SYNOPSIS

    package t::mytest;

    use parent qw( Test::Class::Tiny );

    __PACKAGE__->runtests() if !caller;

    sub T_startup_something {
        # Runs at the start of the test run.
    }

    sub T_setup_something {
        # Runs before each normal test function
    }

    # Expects 2 assertions:
    sub T2_normal {
        ok(1, 'yes');
        ok( !0, 'no');
    }

    # Ignores assertion count:
    sub T0_whatever {
        ok(1, 'yes');
    }

    sub T_teardown_something {
        # Runs after each normal test function
    }

    sub T_shutdown_something {
        # Runs at the end of the test run.
    }

=head1 DESCRIPTION

L<Test::Class> has served Perl’s xUnit needs for a long time
but is incompatible with the L<Test2> framework. This module allows for
a similar workflow but in a way that works with both L<Test2> and the older,
L<Test::Builder>-based modules.

=head1 HOW (AND WHY) TO USE THIS MODULE

xUnit encourages well-designed tests by encouraging creation of independent
chunks of test logic rather than a single monolithic block of code.
xUnit provides standard hooks for:

=over

=item * startup: The start of all tests

=item * setup: The start of an individual test group (i.e., Perl function)

=item * teardown: The end of an individual test group

=item * shutdown: The end of all tests

=back

To write functions that execute at these points in the workflow,
name those functions with the prefixes C<T_startup_>, C<T_setup_>,
C<T_teardown_>, or C<T_shutdown_>.

To write a test function—i.e., a function that actually run some
tests—prefix the function name with C<T>, the number of test assertions
in the function, then an underscore. For example, a function that contains
9 assertions might be named C<T9_check_validation()>. If that function
doesn’t run exactly 9 assertions, a test failure is produced.

(To forgo counting test assertions, use 0 as the test count, e.g.,
C<T0_check_validation()>.)

The above convention is a significant departure from L<Test::Class>,
which uses Perl subroutine attributes to indicate this information.
Using method names is dramatically simpler to implement and also easier
to type.

In most other respects this module attempts to imitate L<Test::Class>.

=head1 TEST INHERITANCE

Like L<Test::Class>, this module seamlessly integrates inherited methods.
To have one test module inherit another module’s tests, just make that
first module a subclass of the latter.

B<CAVEAT EMPTOR:> Inheritance in tests, while occasionally useful, can also
make for difficult maintenance over time if overused. Where I’ve found it
most useful is cases like L<Promise::ES6>, where each test needs to run with
each backend implementation.

=head1 RUNNING YOUR TEST

To use this module to write normal Perl test scripts, just define
the script’s package (ideally not C<main>, but it’ll work) as a subclass of
this module. Then put the following somewhere in the script:

    __PACKAGE__->runtests() if !caller;

Your test will thus execute as a “modulino”.

=head1 SPECIAL FEATURES

=over

=item * As in L<Test::Class>, a C<SKIP_CLASS()> method may be defined. If this
method returns truthy, then the class’s tests are skipped, and that truthy
return is given as the reason for the skip.

=item * The C<TEST_METHOD> environment variable is honored as in L<Test::Class>.

=item * L<Test::Class>’s C<fail_if_returned_early()> method is NOT recognized
here because an early return will already trigger a failure.

=item * Within a test method, C<num_tests()> may be called to retrieve the
number of expected test assertions.

=item * To define a test function whose test count isn’t known until runtime,
name it B<without> the usual C<T$num> prefix, then at runtime do:

    $test_obj->num_method_tests( $name, $count )

See F<t/> in the distribution for an example of this.

=back

=head1 SEE ALSO

Besides L<Test::Class>, you might also look at the following:

=over

=item * L<Test2::Tools::xUnit> also implements xUnit for L<Test2> but doesn’t
allow inheritance.

=item * L<Test::Class::Moose> works with L<Test2>, but the L<Moose> requirement
makes use in CPAN modules problematic.

=back

=head1 AUTHOR

Copyright 2019 L<Gasper Software Consulting|http://gaspersoftware.com> (FELIPE)

=head1 LICENSE

This code is licensed under the same license as Perl itself.

=cut

#----------------------------------------------------------------------

use mro ();

use Test2::API ();

#----------------------------------------------------------------------

use constant SKIP_CLASS => ();

sub new { bless {}, shift }

sub num_tests {
    my ($self) = @_;

    if (!$self->{'_running'}) {
        die "num_tests() called outside of running test!";
    }

    return $self->{'_num_tests'};
}

sub num_method_tests {
    my ($self, $name, $count) = @_;

    die 'need name!' if !$name;

    if (@_ == 2) {
        return $self->{'test'}{$name};
    }

    $self->{'test'}{$name}{'count'} = $count;
    $self->{'test'}{$name}{'simple_name'} = $name;

    return $self;
}

sub runtests {
    my ($self) = @_;

    local $self->{'_running'} = 1;

    # Allow calls as either instance or object method.
    if (!ref $self) {
        my $obj = $self->new();
        $self = $obj;
    }

    my $ctx = Test2::API::context();

    if (my $reason = $self->SKIP_CLASS()) {
        my $ctx = Test2::API::context();
        $ctx->plan(1);
        $ctx->skip( ref($self), $reason );
    }
    else {
        $self->_analyze();

        if ( my $startup_hr = $self->{'startup'} ) {
            $self->_run_funcs($startup_hr);
        }

        my $setup_hr = $self->{'setup'};
        my $teardown_hr = $self->{'teardown'};

        if ( my $tests_hr = $self->{'test'} ) {
            for my $fn (sort keys %$tests_hr) {

                if (my $ptn = $ENV{'TEST_METHOD'}) {
                    next if $fn !~ m<$ptn>;
                }

                if ($ENV{'TEST_VERBOSE'}) {
                    $ctx->diag( $/ . ref($self) . "->$fn()" );
                }

                $self->_run_funcs($setup_hr);

                my $want_count = $tests_hr->{$fn}{'count'};

                local $self->{'_num_tests'} = $want_count;

                my ($died_yn, $err);

                my $events_ar = Test2::API::intercept( sub {
                    eval { $self->$fn(); 1 } or do {
                        $err = $@;

                        $died_yn = 1;
                    };
                } );

                for my $event (@$events_ar) {
                    if ($event->can('name') && !defined $event->name()) {
                        my $name = $tests_hr->{$fn}{'simple_name'};
                        $name =~ tr<_>< >;
                        $event->set_name($name);
                    }

                    $ctx->hub()->send($event);
                }

                $ctx->fail("$fn()", "Caught exception: $err") if $died_yn;

                if ($want_count) {
                    my $got_count = 0;
                    $got_count++ for grep { $_->increments_count() } @$events_ar;

                    if ($want_count != $got_count) {
                        $ctx->fail("Test count mismatch: got $got_count, expected $want_count");
                    }
                }

                $self->_run_funcs($teardown_hr);
            }
        }

        if ( my $shutdown_hr = $self->{'shutdown'} ) {
            $self->_run_funcs($shutdown_hr);
        }
    }

    $ctx->done_testing();

    $ctx->release();

    return;
}

sub _analyze {
    my ($self) = @_;

    if (!$self->{'_analyzed'}) {
        my @isa = @{ mro::get_linear_isa(ref $self) };

        for my $ns (@isa) {
            my $ptbl_hr = do {
                no strict 'refs';
                \%{"${ns}::"};
            };

            for my $name (keys %$ptbl_hr) {
                next if !$self->can($name);
                next if $name !~ m<\AT(_setup|_teardown|_startup|_shutdown|[0-9]+)_(.+)>;

                my $whatsit = $1;
                my $simple_name = $2;

                if ( $whatsit =~ s<\A_><>) {
                    $self->{$whatsit}{$name} = undef;
                }
                else {
                    $self->{'test'}{$name} = {
                        count => $whatsit,
                        simple_name => $simple_name,
                    };
                }
            }
        }

        $self->{'_analyzed'} = 1;
    }

    return;
}

sub _run_funcs {
    my ($self, $funcs_hr) = @_;

    for my $fn (sort keys %$funcs_hr) {
        $funcs_hr->{$fn}->($self);
    }

    return;
}

1;
