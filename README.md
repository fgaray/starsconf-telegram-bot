# StarsConf bot

This is a bot for the [StarsConf 2017](www.starsconf.com). The
[challenge](https://medium.com/@SynapticSpa/construye-un-bot-para-la-starsconf-y-gana-una-entrada-d16cb0e24143)
consists in building a bot to notify the registered users when a talk is going
to start.


# How to install

You need to install Haskell's
[stack](https://docs.haskellstack.org/en/stable/README/#how-to-install) in order
to build this project.

## Steps

* Clone this repository.
* Run **stack setup** inside the cloned folder.
* Run **stack build** to create a binary exe.
* Inside $PWD/.stack-work/install/x86_64-linux-nopie/lts-9.4/8.0.2/bin should be
  an executable called **starsconf-bot-exe**.

# How to run

Just run in the console TOKEN=YOUR-BOT-TOKEN ./starsconf-bot-exe
