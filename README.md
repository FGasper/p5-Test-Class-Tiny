# NAME

Test::Class::Tiny - xUnit in Perl, simplified

# SYNOPSIS

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

# DESCRIPTION

[Test::Class](https://metacpan.org/pod/Test::Class) has served Perl’s xUnit needs for a long time
but is incompatible with the [Test2](https://metacpan.org/pod/Test2) framework. This module allows for
a similar workflow but in a way that works with both [Test2](https://metacpan.org/pod/Test2) and the older,
[Test::Builder](https://metacpan.org/pod/Test::Builder)-based modules.

# HOW (AND WHY) TO USE THIS MODULE

xUnit encourages well-designed tests by encouraging creation of independent
chunks of test logic rather than a single monolithic block of code.
xUnit provides standard hooks for:

- startup: The start of all tests
- setup: The start of an individual test group (i.e., Perl function)
- teardown: The end of an individual test group
- shutdown: The end of all tests

To write functions that execute at these points in the workflow,
name those functions with the prefixes `T_startup_`, `T_setup_`,
`T_teardown_`, or `T_shutdown_`.

To write a test function—i.e., a function that actually run some
tests—prefix the function name with `T`, the number of test assertions
in the function, then an underscore. For example, a function that contains
9 assertions might be named `T9_check_validation()`. If that function
doesn’t run exactly 9 assertions, a test failure is produced.

(To forgo counting test assertions, use 0 as the test count, e.g.,
`T0_check_validation()`.)

The above convention is a significant departure from [Test::Class](https://metacpan.org/pod/Test::Class),
which uses Perl subroutine attributes to indicate this information.
Using method names is dramatically simpler to implement and also easier
to type.

In most other respects this module attempts to imitate [Test::Class](https://metacpan.org/pod/Test::Class).

# TEST INHERITANCE

Like [Test::Class](https://metacpan.org/pod/Test::Class), this module seamlessly integrates inherited methods.
To have one test module inherit another module’s tests, just make that
first module a subclass of the latter.

**CAVEAT EMPTOR:** Inheritance in tests, while occasionally useful, can also
make for difficult maintenance over time if overused. Where I’ve found it
most useful is cases like [Promise::ES6](https://metacpan.org/pod/Promise::ES6), where each test needs to run with
each backend implementation.

# RUNNING YOUR TEST

To use this module to write normal Perl test scripts, just define
the script’s package (ideally not `main`, but it’ll work) as a subclass of
this module. Then put the following somewhere in the script:

    __PACKAGE__->runtests() if !caller;

Your test will thus execute as a “modulino”.

# SPECIAL FEATURES

- As in [Test::Class](https://metacpan.org/pod/Test::Class), a `SKIP_CLASS()` method may be defined. If this
method returns truthy, then the class’s tests are skipped, and that truthy
return is given as the reason for the skip.
- The `TEST_METHOD` environment variable is honored as in [Test::Class](https://metacpan.org/pod/Test::Class).
- [Test::Class](https://metacpan.org/pod/Test::Class)’s `fail_if_returned_early()` method is NOT recognized
here because an early return will already trigger a failure.
- Within a test method, `num_tests()` may be called to retrieve the
number of expected test assertions.
- To define a test function whose test count isn’t known until runtime,
name it **without** the usual `T$num` prefix, then at runtime do:

        $test_obj->num_method_tests( $name, $count )

    See `t/` in the distribution for an example of this.

# SEE ALSO

Besides [Test::Class](https://metacpan.org/pod/Test::Class), you might also look at the following:

- [Test2::Tools::xUnit](https://metacpan.org/pod/Test2::Tools::xUnit) also implements xUnit for [Test2](https://metacpan.org/pod/Test2) but doesn’t
allow inheritance.
- [Test::Class::Moose](https://metacpan.org/pod/Test::Class::Moose) works with [Test2](https://metacpan.org/pod/Test2), but the [Moose](https://metacpan.org/pod/Moose) requirement
makes use in CPAN modules problematic.

# AUTHOR

Copyright 2019 [Gasper Software Consulting](http://gaspersoftware.com) (FELIPE)

# LICENSE

This code is licensed under the same license as Perl itself.