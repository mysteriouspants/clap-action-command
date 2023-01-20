# Clap Action Command

A command-map pattern for easily configuring and dispatching modular
subcommands using the Clap argument parser. This makes building and modifying
large CLI projects more ergonomic by reducing the touch points required to
add or modify additional subcommands.

Define modular subcommands by implementing the `ActionCommand` trait. Dispatch
to the modular subcommand by placing it in a `CommandMap`.

```rust
use std::error::Error;
use clap::{Arg, ArgMatches, builder::NonEmptyStringValueParser, Command};
use clap_action_command::{
    ActionCommand, CommandMap, get_one, vec1::{Vec1, vec1}
};

static NAME_ARG: &str = "name";

struct HelloWorldCommand {}

impl ActionCommand for HelloWorldCommand {
    fn name(&self) -> &'static str {
        "hello-world"
    }

    fn command(&self, command: Command) -> Command {
        command
            .about("Say hello to the world")
            .alias("h")
            .arg(
                Arg::new(NAME_ARG)
                    .short('n')
                    .value_name("NAME")
                    .required(false)
                    .value_parser(NonEmptyStringValueParser::new())
            )
    }

    fn action(
            &self, matches: Vec1<&ArgMatches>
    ) -> Result<(), Box<dyn Error>> {
        if let Some(name) = get_one::<String>(&matches, NAME_ARG) {
            println!("Hello, {}!", name);
        } else {
            println!("Hello, world!");
        }

        Ok(())
    }
}

let command_map = CommandMap::builder()
    .push(HelloWorldCommand {})
    // adding a new subcommand is as easy as referencing it here
    .build();
let command = Command::new("my-program").subcommands(command_map.commands());
let matches = command.get_matches_from([
    "my-program", "hello-world", "-n", "Steeve"
]);
command_map.dispatch(vec1![&matches]);
```

## On Asynchrony

Tokio examples place an attribute on the `main` function.

```ignore
#[tokio::main]
fn main() {
    println!("Hello, world!");
}
```

`ActionCommand`, however, is not asynchronous. Move the Tokio attribute below
the `ActionCommand` to use `ActionCommand` to start an asynchronous program.

```ignore
struct AsyncCommand {}

impl AsyncCommand {
    #[tokio::main]
    async fn async_action(&self) -> Result<(), Error> {
        println!("Hello, world!");

        Ok(())
    }
}

impl ActionCommand for AsyncCommand {
    fn name(&self) -> &'static str {
        "async-command"
    }

    fn command(&self, command: Command) -> Command {
        command
            .about("Say hello to the world")
    }

    fn action(
        &self, matches: Vec1<&ArgMatches>,
    ) -> Result<(), Box<dyn Error>> {
        self.async_action()?;

        Ok(())
    }
}
```

## License

I want you to be able to use this software regardless of who you may be, what
you are working on, or the environment in which you are working on it - I hope
you'll use it for good and not evil! To this end, the Clap Action Command
source code is licensed under the 1-clause BSD license, with other licenses
available by request. Happy coding!
