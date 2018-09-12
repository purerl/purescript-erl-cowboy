# purescript-erl-cowboy

Bindings to the [cowboy](https://github.com/ninenines/cowboy) web server. Bindings tested at version `2.4`.


## Usage

Firstly this package contains bindings to `cowboy`, it must be used in an OTP application where `cowboy` is installed
at a suitable version.

To construct a working cowboy application, the definitions in `Erl.Cowboy` can be used with 
routing defined in `Erl.Cowboy.Routes`, and one of the handlers defind in submodules of
`Erl.Cowboy.Handlers`. Core request processing is handled in `Erl.Cowboy.Req`.

Examples can be found in the [pscowboytest](https://github.com/nwolverson/pscowboytest) test project.