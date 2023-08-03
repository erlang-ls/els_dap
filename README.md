# els_dap

![erlang_ls](images/erlang-ls-logo-small.png?raw=true "Erlang LS")

A standalone version of the DAP debugger originally included as part of Erlang LS.

## Minimum Requirements

* [Erlang OTP 22+](https://github.com/erlang/otp)
* [rebar3 3.9.1+](https://github.com/erlang/rebar3)

## Quickstart

Compile the project:

    make

To install the produced `els_dap` escript in `/usr/local/bin`:

    make install

To install to a different directory set the `PREFIX` environment variable:

    PREFIX=/path/to/directory make install

## Getting in Touch

If you have any questions about the project, feel free to open a new
issue. You can also join the `#erlang-ls` channel in the
_Erlanger_ Slack if you would like to get involved or if you prefer a
more informal mean of communication.

All contributions are welcome, be them in the form of a bug report, a
question, feedback, or code.

I occasionally blog about the project on
[Medium](https://medium.com/about-erlang).

## License

The `els_dap` project is licensed under the Apache License 2.0. Please refer
to the `LICENSE` file for details.
