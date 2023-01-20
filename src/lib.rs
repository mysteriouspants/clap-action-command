//! A command-map pattern for easily configuring and dispatching modular
//! subcommands using the Clap argument parser.
//!
//! The first part is the [`ActionCommand`] trait, which is implemented
//! to associate a behavior with a [`clap::Command`].
//!
//! ```rust
//! # use clap::{
//! #     Arg, ArgMatches, Command,
//! #     builder::NonEmptyStringValueParser,
//! # };
//! # use vec1::Vec1;
//! # use clap_action_command::{ActionCommand, get_one};
//! static NAME_ARG: &str = "name";
//!
//! struct HelloWorldCommand {}
//!
//! impl ActionCommand for HelloWorldCommand {
//!     // This specifies the name of the ActionCommand, so hello-world
//!     // here would be invokable as my-program hello-world if it were
//!     // to be run from a binary called my-program.
//!     fn name(&self) -> &'static str {
//!         "hello-world"
//!     }
//!
//!     // This is the command builder. The command itself is configured
//!     // with the name given previously, and here it may be configured
//!     // with additional args or aliases.
//!     fn command(&self, command: Command) -> Command {
//!         command
//!             .about("Say hello to the world")
//!             .alias("h")
//!             .arg(
//!                 Arg::new(NAME_ARG)
//!                     .short('n')
//!                     .value_name("NAME")
//!                     .required(false)
//!                     .value_parser(NonEmptyStringValueParser::new())
//!             )
//!     }
//!
//!     // The action to take when the ActionCommand is matched, given
//!     // a list of all (sub)commands for argument/flag retrieval.
//!     fn action(
//!         &self, matches: Vec1<&ArgMatches>
//!     ) -> Result<(), Box<dyn std::error::Error>> {
//!         if let Some(name_arg) =
//!                 get_one::<String>(&matches, NAME_ARG) {
//!             println!("Hello, {}!", name_arg);
//!         } else {
//!             println!("Hello, world!");
//!         }
//!
//!         Ok(())
//!     }
//! }
//! ```
//!
//! The second part is the [`CommandMap`] type, which aggregates one or
//! more [`ActionCommand`]s and handles dispatching to them based on
//! a [`clap::ArgMatches`].
//!
//! ```rust
//! # use clap::{ArgMatches, Command, command};
//! # use vec1::vec1;
//! # use clap_action_command::{
//! #     ActionCommand, CommandMap, HelloWorldCommand
//! # };
//! #
//! // Add commands to the map by pushing ActionCommand trait objects
//! // onto the map
//! let command_map = CommandMap::builder()
//!     .push(HelloWorldCommand {})
//!     .build();
//! // Tell the relevant Clap Command about all the subcommands
//! let command = Command::new("my-program")
//!     .subcommands(command_map.commands());
//! let matches = command.get_matches_from(
//!     ["my-program", "hello-world"]
//! );
//! // Dispatch to the relevant subcommand, saving some if/else-if
//! // chains
//! command_map.dispatch(vec1![&matches]);
//! ```

pub use vec1;

use clap::{
    parser::{RawValues, ValueSource, ValuesRef},
    ArgMatches, Command,
};
use snafu::{ResultExt, Snafu};
use std::{any::Any, collections::HashMap, error::Error, iter::Flatten, vec::IntoIter};
use vec1::Vec1;

/// A type that encapsulates the Clap [`Command`] and associated
/// behavior of that command when it is matched.
///
/// ```rust
/// # use clap::{ArgMatches, Command};
/// # use clap_action_command::ActionCommand;
/// # use vec1::Vec1;
/// #
/// struct HelloWorldCommand {}
///
/// impl ActionCommand for HelloWorldCommand {
///     fn name(&self) -> &'static str {
///         "hello-world"
///     }
///
///     fn command(&self, command: Command) -> Command {
///         command
///             .about("Say hello to the world")
///             .alias("h")
///     }
///
///     fn action(
///         &self, _matches: Vec1<&ArgMatches>,
///     ) -> Result<(), Box<dyn std::error::Error>> {
///         println!("Hello, world!");
///         Ok(())
///     }
/// }
/// ```
pub trait ActionCommand<T = (), E = Box<dyn Error>> {
    /// The name of the command.
    fn name(&self) -> &'static str;

    /// The [`Command`] that describes how to match this on the command
    /// line using Clap. `command` is already constructed using
    /// [`Self::name`] for convenience.
    fn command(&self, command: Command) -> Command;

    /// The action to take when this command is matched on the command
    /// line. [`CommandMap`]s may be nested, and this is represented
    /// by the matches being returned as a list of at least one element.
    fn action(&self, matches: Vec1<&ArgMatches>) -> Result<T, E>;
}

/// A type which has a set of [`ActionCommand`]s and can provide the
/// Clap [`Command`] for command line arg parsing, as well as map a
/// matched [`Command`] back to its [`ActionCommand`] for dispatch to
/// its action function.
///
/// ```rust
/// # use clap::{ArgMatches, Command, command};
/// # use vec1::vec1;
/// # use clap_action_command::{
/// #     ActionCommand, CommandMap, HelloWorldCommand
/// # };
/// #
/// // Add commands to the map by pushing ActionCommand trait objects
/// // onto the map
/// let command_map = CommandMap::builder()
///     .push(HelloWorldCommand {})
///     .build();
/// // Tell the relevant Clap Command about all the subcommands
/// let command = Command::new("my-program")
///     .subcommands(command_map.commands());
/// let matches = command.get_matches_from(
///     ["my-program", "hello-world"]
/// );
/// // Dispatch to the relevant subcommand, saving some if/else-if
/// // chains
/// command_map.dispatch(vec1![&matches]);
/// ```
///
/// This type can be composed, for example on a subcommand with multiple
/// subcommands of its own. See [`CommandMapActionCommand`] for a
/// minimal example.
pub struct CommandMap<'a, T = (), E = Box<dyn Error>> {
    command_map: HashMap<&'static str, Box<dyn ActionCommand<T, E> + Send + Sync + 'a>>,
}

impl<'a> CommandMap<'a> {
    /// Creates a builder type which is used to tell the [`CommandMap`]
    /// about the [`ActionCommand`]s it will be mapping over.
    pub fn builder() -> CommandMapBuilder<'a> {
        CommandMapBuilder {
            command_map: HashMap::new(),
        }
    }

    /// The Clap [`Command`]s for this [`CommandMap`]. Use this with
    /// [`clap::Command::subcommands`] to configure it to use this
    /// [`CommandMap`].
    pub fn commands(&self) -> Vec<Command> {
        self.command_map
            .values()
            .map(|v| v.command(Command::new(v.name())))
            .collect()
    }

    /// Dispatch this [`CommandMap`] using [`ArgMatches`].
    ///
    /// When starting from scratch simply construct a new
    /// [`vec1::Vec1`] with a single [`clap::ArgMatches`] in it; when
    /// nesting multiple [`CommandMap`]s it is helpful to keep the
    /// previous subcommand stack accessible by extending the matches
    /// vector using [`vec1::Vec1::from_vec_push`].
    pub fn dispatch(&self, matches: Vec1<&ArgMatches>) -> Result<(), DispatchError> {
        let local_matches = matches.last();
        if let Some((command_name, subcommand)) = local_matches.subcommand() {
            if let Some(action_command) = self.command_map.get(command_name) {
                action_command
                    .action(Vec1::from_vec_push(matches.to_vec(), subcommand))
                    .with_context(|_| ActionCommandSnafu {
                        command_name: command_name.to_owned(),
                    })?;

                return Ok(());
            }

            return Err(DispatchError::SubcommandNotInMap {
                command_name: command_name.to_owned(),
                all_commands: self
                    .command_map
                    .values()
                    .map(|action_command| action_command.name())
                    .collect(),
            });
        }

        Err(DispatchError::NoSubcommand)
    }
}

/// Error generated by [`CommandMap::dispatch`].
#[derive(Debug, Snafu)]
pub enum DispatchError {
    /// An error originating from the execution of the [`ActionCommand`]
    /// itself - an error in the business logic.
    ActionCommand {
        command_name: String,
        source: Box<dyn std::error::Error>,
    },

    /// The [`CommandMap`] does not have an associated [`ActionCommand`]
    /// named the same thing as the [`clap::ArgMatches`] matched
    /// command. This may happen if additional [`clap::Command`]s have
    /// been added beyond those present in the [`CommandMap`].
    SubcommandNotInMap {
        command_name: String,
        all_commands: Vec<&'static str>,
    },

    /// The [`clap::ArgMatches`] does not have a subcommand, which means
    /// that the [`clap::Command`] which matched this
    /// [`clap::ArgMatches`] is the most specific. For example, if
    /// `my-program` has a `hello-world` subcommand, but the
    /// [`CommandMap`] returns [`DispatchError::NoSubcommand`], it means
    /// that the program was invoked as `my-program` with no subcommand
    /// at all.
    NoSubcommand,
}

/// Used to fluently construct [`CommandMap`]s.
pub struct CommandMapBuilder<'a> {
    command_map: HashMap<&'static str, Box<dyn ActionCommand + Send + Sync + 'a>>,
}

impl<'a> CommandMapBuilder<'a> {
    /// Add a new [`ActionCommand`] to the [`CommandMap`].
    pub fn push(
        mut self,
        action_command: impl ActionCommand + Send + Sync + 'a,
    ) -> CommandMapBuilder<'a> {
        self.command_map
            .insert(action_command.name(), Box::new(action_command));

        self
    }

    /// Add zero or more [`ActionCommand`]s to the [`CommandMap`].
    pub fn push_all(
        mut self,
        action_commands: impl IntoIterator<Item = impl ActionCommand + Send + Sync + 'a>,
    ) -> CommandMapBuilder<'a> {
        for action_command in action_commands {
            self.command_map
                .insert(action_command.name(), Box::new(action_command));
        }

        self
    }

    /// Finalize this builder and generate a [`CommandMap`].
    pub fn build(self) -> CommandMap<'a> {
        CommandMap {
            command_map: self.command_map,
        }
    }
}

/// An [`ActionCommand`] that only composes a [`CommandMap`]'s
/// subcommands. Any customization of the [`clap::Command`] or the
/// behavior of [`CommandMapActionCommand::action`] will require a
/// custom type.
///
/// ```rust
/// # use clap_action_command::{
/// #     CommandMap, CommandMapActionCommand, HelloWorldCommand
/// # };
/// // create a new CommandMapActionCommand, including all of the
/// // subcommands it must dispatch to.
/// let foo = CommandMapActionCommand::new(
///     "foo",
///     CommandMap::builder()
///         .push(HelloWorldCommand {})
///         .build(),
/// );
/// // add that CommandMapActionCommand to its parent CommandMap, which
/// // will automatically dispatch and route to subcommands
/// let command_map = CommandMap::builder()
///     .push(foo)
///     .build();
/// ```
pub struct CommandMapActionCommand<'a> {
    name: &'static str,
    command_map: CommandMap<'a>,
}

impl<'a> CommandMapActionCommand<'a> {
    /// Create a new [`CommandMapActionCommand`] give a name and a
    /// [`CommandMap`].
    pub fn new(name: &'static str, command_map: CommandMap<'a>) -> CommandMapActionCommand<'a> {
        CommandMapActionCommand { name, command_map }
    }
}

impl<'a> ActionCommand for CommandMapActionCommand<'a> {
    fn name(&self) -> &'static str {
        self.name
    }

    fn command(&self, command: clap::Command) -> clap::Command {
        command.subcommands(self.command_map.commands())
    }

    fn action(&self, matches: Vec1<&ArgMatches>) -> Result<(), Box<dyn std::error::Error>> {
        match self.command_map.dispatch(matches) {
            Ok(()) => Ok(()),
            Err(e) => Err(Box::new(e)),
        }
    }
}

/// Helper function for dealing with chains of [`ArgMatches`] while
/// working in [`ActionCommand::action`] to find arguments which may
/// have been spcified anywhere in the subcommand tree.
///
/// ```rust
/// # use clap_action_command::get_many;
/// # use clap::{Arg, Command};
/// #
/// # let command = Command::new("my-program")
/// #     .arg(Arg::new("my-arg").long("my-arg"))
/// #     .subcommand(
/// #           Command::new("my-subcommand")
/// #               .arg(Arg::new("my-arg").long("my-arg")));
/// # let matches = command.get_matches_from([
/// #     "my-program",
/// #     "--my-arg",
/// #     "alpha",
/// #     "my-subcommand",
/// #     "--my-arg",
/// #     "beta",
/// # ]);
/// # let matches = vec![&matches, matches.subcommand().unwrap().1];
/// #
/// // my-program --my-arg alpha my-subcommand --my-arg beta
/// let arg = get_many::<String>(&matches, "my-arg");
/// assert_eq!(vec!["alpha", "beta"], arg.collect::<Vec<_>>());
/// ```
pub fn get_many<'a, T: Any + Clone + Send + Sync + 'static>(
    matches: &[&'a ArgMatches],
    id: &str,
) -> Flatten<IntoIter<ValuesRef<'a, T>>> {
    let mut collected_values = vec![];

    for matches in matches.iter() {
        if let Ok(Some(values)) = matches.try_get_many(id) {
            collected_values.push(values);
        }
    }

    collected_values.into_iter().flatten()
}

/// Helper function for dealing with chains of [`ArgMatches`] while
/// working in [`ActionCommand::action`] to find arguments which may
/// have been specified anywhere in the subcommand tree.
///
/// ```rust
/// # use clap_action_command::get_one;
/// # use clap::{Arg, Command};
/// #
/// # let command = Command::new("my-program")
/// #     .arg(Arg::new("my-arg").long("my-arg"))
/// #     .subcommand(
/// #           Command::new("my-subcommand")
/// #               .arg(Arg::new("my-arg").long("my-arg")));
/// # let matches = command.get_matches_from([
/// #     "my-program",
/// #     "--my-arg",
/// #     "alpha",
/// #     "my-subcommand",
/// #     "--my-arg",
/// #     "beta",
/// # ]);
/// # let matches = vec![&matches, matches.subcommand().unwrap().1];
/// #
/// // my-program --my-arg alpha my-subcommand --my-arg beta
/// let arg = get_one::<String>(&matches, "my-arg");
/// assert_eq!("beta", arg.unwrap());
/// ```
///
/// This function respects the provenance
/// ([`clap::parser::ValueSource`]) of arguments. For example, a default
/// or environment-sourced value will never override a value specified
/// explicitly on the command line.
pub fn get_one<'a, T: Any + Clone + Send + Sync + 'static>(
    matches: &[&'a ArgMatches],
    id: &str,
) -> Option<&'a T> {
    let mut best_match = None;
    let mut best_match_specificity = ValueSource::DefaultValue;

    for matches in matches.iter() {
        let current_match = match matches.try_get_one::<T>(id) {
            Ok(arg_match) => arg_match,
            Err(_) => continue,
        };
        let current_specificity = matches.value_source(id);

        if let Some(current_specificity) = current_specificity {
            if best_match_specificity <= current_specificity {
                best_match = current_match;
                best_match_specificity = current_specificity;
            }
        }
    }

    best_match
}

/// Helper function for dealing with chains of [`ArgMatches`] while
/// working in [`ActionCommand::action`] to find arguments which may
/// have been specified anywhere in the subcommand tree.
///
/// ```rust
/// # use clap_action_command::get_raw;
/// # use clap::{Arg, Command};
/// # use std::ffi::OsStr;
/// #
/// # let command = Command::new("my-program")
/// #     .arg(Arg::new("my-arg").long("my-arg"))
/// #     .subcommand(
/// #           Command::new("my-subcommand")
/// #               .arg(Arg::new("my-arg").long("my-arg")));
/// # let matches = command.get_matches_from([
/// #     "my-program",
/// #     "--my-arg",
/// #     "alpha",
/// #     "my-subcommand",
/// #     "--my-arg",
/// #     "beta",
/// # ]);
/// # let matches = vec![&matches, matches.subcommand().unwrap().1];
/// #
/// // my-program --my-arg alpha my-subcommand --my-arg beta
/// let arg = get_raw(&matches, "my-arg");
/// assert_eq!(
///     vec![OsStr::new("alpha"), OsStr::new("beta")],
///     arg.collect::<Vec<_>>(),
/// );
/// ```
pub fn get_raw<'a>(matches: &[&'a ArgMatches], id: &str) -> Flatten<IntoIter<RawValues<'a>>> {
    let mut collected_values = vec![];

    for matches in matches.iter() {
        if let Ok(Some(values)) = matches.try_get_raw(id) {
            collected_values.push(values);
        }
    }

    collected_values.into_iter().flatten()
}

// just here to clean up some doctests
// TODO: can this be turned off in normal builds with eg a
//       #[cfg(doctest)]?
#[doc(hidden)]
pub struct HelloWorldCommand {}

impl ActionCommand for HelloWorldCommand {
    fn name(&self) -> &'static str {
        "hello-world"
    }

    fn command(&self, command: Command) -> Command {
        command.about("Say hello to the world").alias("h")
    }

    fn action(&self, _matches: Vec1<&ArgMatches>) -> Result<(), Box<dyn std::error::Error>> {
        println!("Hello, world!");
        Ok(())
    }
}

#[doc = include_str!("../README.md")]
#[cfg(doctest)]
struct ReadmeDoctests {}

#[cfg(test)]
mod tests {
    use super::{get_one, ActionCommand, CommandMap, CommandMapActionCommand, DispatchError};
    use clap::{builder::NonEmptyStringValueParser, Arg, ArgMatches, Command};
    use std::ffi::OsString;
    use vec1::{vec1, Vec1};

    struct HelloWorldCommand {}

    impl ActionCommand for HelloWorldCommand {
        fn name(&self) -> &'static str {
            "hello-world"
        }

        fn command(&self, command: clap::Command) -> clap::Command {
            command.alias("h").arg(
                Arg::new("bar")
                    .short('b')
                    .value_parser(NonEmptyStringValueParser::new())
                    .required(true),
            )
        }

        fn action(&self, matches: Vec1<&ArgMatches>) -> Result<(), Box<dyn std::error::Error>> {
            println!(
                "Hello, World! My args are {{ foo: {}, bar: {} }}",
                matches.first().get_one::<String>("foo").unwrap(),
                matches.last().get_one::<String>("bar").unwrap(),
            );
            Ok(())
        }
    }

    fn example_dispatch(
        itr: impl IntoIterator<Item = impl Into<OsString> + Clone>,
    ) -> Result<(), DispatchError> {
        let base_command = Command::new("command_matching").arg(
            Arg::new("foo")
                .short('f')
                .value_parser(NonEmptyStringValueParser::new())
                .required(true),
        );
        let command_map_action_command = CommandMapActionCommand::new(
            "foo",
            CommandMap::builder().push(HelloWorldCommand {}).build(),
        );
        let command_map = CommandMap::builder()
            .push(command_map_action_command)
            .build();
        let base_command = base_command
            .subcommands(command_map.commands())
            .subcommand(Command::new("bar"));
        let matches = base_command.get_matches_from(itr);

        command_map.dispatch(vec1![&matches])
    }

    // --------------------------------------------------------------
    // these tests are too large for the CommandMap doctest, and they
    // generally focus on failure modes or subtleties not appropriate
    // for the brevity of docs
    // --------------------------------------------------------------

    #[test]
    fn alias_matching() {
        let r = example_dispatch([
            "command_matching",
            "-f",
            "my_foo",
            "foo",
            "h", // use an alias for a command rather than its full name
            "-b",
            "my_bar",
        ]);

        assert!(r.is_ok());
    }

    #[test]
    fn subcommand_not_in_map() {
        let r = example_dispatch(["command_matching", "-f", "my_foo", "bar"]);

        assert!(matches!(
            r,
            Err(DispatchError::SubcommandNotInMap {
                command_name: _,
                all_commands: _,
            })
        ));
    }

    #[test]
    fn no_subcommand() {
        let r = example_dispatch(["command_matching", "-f", "my_foo"]);

        assert!(matches!(r, Err(DispatchError::NoSubcommand)));
    }

    #[test]
    fn get_one_picks_most_specific() {
        let command = Command::new("my-program")
            .arg(Arg::new("my-arg").long("my-arg").default_value("gamma"))
            .subcommand(Command::new("my-subcommand").arg(Arg::new("my-arg").long("my-arg")));
        let matches = command.get_matches_from([
            "my-program",
            "--my-arg",
            "alpha",
            "my-subcommand",
            "--my-arg",
            "beta",
        ]);
        let command_matches = vec![&matches, matches.subcommand().unwrap().1];
        let arg = get_one::<String>(&command_matches, "my-arg");

        assert_eq!("beta", arg.unwrap());
    }

    #[test]
    fn get_one_ignores_defaults() {
        let command = Command::new("my-program")
            .arg(Arg::new("my-arg").long("my-arg").default_value("gamma"))
            .subcommand(Command::new("my-subcommand").arg(Arg::new("my-arg").long("my-arg")));
        let matches = command.get_matches_from([
            "my-program",
            // --my-arg defaults to gamma
            "my-subcommand",
            "--my-arg",
            "beta",
        ]);
        let command_matches = vec![&matches, matches.subcommand().unwrap().1];
        let arg = get_one::<String>(&command_matches, "my-arg");

        assert_eq!("beta", arg.unwrap());
    }
}
