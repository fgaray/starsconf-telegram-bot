# StarsConf bot

This is a bot for the [StarsConf 2017](www.starsconf.com). The
[challenge](https://medium.com/@SynapticSpa/construye-un-bot-para-la-starsconf-y-gana-una-entrada-d16cb0e24143)
consists in building a bot to notify the registered users when a talk is going
to start.


You can chat with the [bot](https://web.telegram.org/#/im?p=@StarsConfBot) in telegram.


# Getting the binary

You can download a
[binary](https://github.com/fgaray/starsconf-telegram-bot/releases/download/0.1/starsconf-bot-exe)
or compile the bot using the following steps from source.

The ldd output of the binary is:

    linux-vdso.so.1 (0x00007ffc4f347000)
    libpthread.so.0 => /lib/x86_64-linux-gnu/libpthread.so.0 (0x00007f6b6e7ef000)
    libz.so.1 => /lib/x86_64-linux-gnu/libz.so.1 (0x00007f6b6e5d5000)
    librt.so.1 => /lib/x86_64-linux-gnu/librt.so.1 (0x00007f6b6e3cd000)
    libutil.so.1 => /lib/x86_64-linux-gnu/libutil.so.1 (0x00007f6b6e1ca000)
    libdl.so.2 => /lib/x86_64-linux-gnu/libdl.so.2 (0x00007f6b6dfc6000)
    libgmp.so.10 => /usr/lib/x86_64-linux-gnu/libgmp.so.10 (0x00007f6b6dd43000)
    libm.so.6 => /lib/x86_64-linux-gnu/libm.so.6 (0x00007f6b6da3f000)
    libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007f6b6d6a2000)
    /lib64/ld-linux-x86-64.so.2 (0x00007f6b6ea0c000)



# How to compile

You need to install Haskell's
[stack](https://docs.haskellstack.org/en/stable/README/#how-to-install) in order
to build this project.

Also, you are going to need to install the sqlite-dev package (in Debian and
Ubuntu) to be able to compile this project.

## Steps

TIP: If you want to speed up the compilation, you can add -j N to stack to use
an N number of process to build the dependencies. For example: stack -j 4 build.

* Clone this repository.
* Run **stack setup** inside the cloned folder.
* Run **stack build** to create a binary exe.
* Inside $PWD/.stack-work/install/x86_64-linux-nopie/lts-9.4/8.0.2/bin should be
  an executable called **starsconf-bot-exe**.

# How to run

Just run in the console TOKEN=BOTID:YOUR-BOT-TOKEN ./starsconf-bot-exe

**REMEMBER TO ADD your bot id to the token, it should be something like TOKEN=bot1234:a1b2c3d4...**

If you compiled the binary from source using stack, you can run the binary
using: TOKEN=BOTID:YOUR-BOT-TOKEN stack exec starsconf-bot-exe

This bot uses an sqlite database to help to store users and cache the API. A
file will be created 


# Commands and Keyboard

This bots uses the Telegram's Keyboard to help the users to send commands to the bot.

It provides two languages: English and Spanish. The bot gives the user this two
options the first time.

You can register to get notifications for the up comming talks, unregister, ask
for next talks and ask for the current talks at that moment.

## Examples

Here are some examples of the responses given by the bot:

![](https://github.com/fgaray/starsconf-telegram-bot/blob/master/images/Captura%20de%20pantalla%20de%202017-10-05%2022-07-49.png?raw=true)
![](https://github.com/fgaray/starsconf-telegram-bot/blob/master/images/Captura%20de%20pantalla%20de%202017-10-05%2023-00-35.png?raw=true)
![](https://github.com/fgaray/starsconf-telegram-bot/blob/master/images/Captura%20de%20pantalla%20de%202017-10-05%2023-01-12.png?raw=true)


# License


[![GNU GPL v3.0](http://www.gnu.org/graphics/gplv3-127x51.png)](http://www.gnu.org/licenses/gpl.html)

View official GNU site <http://www.gnu.org/licenses/gpl.html>.
