#  LSM: The Local Service Manager

LSM is small process supervision tool which allows the maintenance of long-living background tasks, or daemons, on unix-like systems. Essentially, it can start/stop/restart/auto restart/check the status of user-defined services. The services can be any executable (including a regress of lsmd instances though this is not advised). A simple tcp client talks on the configured address and port to instruct lsmd to manage services. So what makes this program different? It's designed to run as a normal user and not root. So for those without systemd, it can replace systemctl --user, and for those without a desktop environment session manager, it can stand in there too.

# Installation

First, install the dependencies, namely sbcl (available on most Linux repositories) and quicklisp (see https://www.quicklisp.org/beta/#installation).

Because LSM is in early alpha, I only provide a public git repository at this time. I also ask for your patience with the inevitable slew of bugs and deficiencies you will find if you install this program. Anyway, you can clone https://github.com/mikhailkfv/lsm.git to download a copy. Then you can simlink lsm and lsmd to a location in your PATH (or compile a binary using (sb-exit:save-lisp-and-die).

# Configuration

Now make a configuration file. The default location is `~/.config/lsmrc` If you want a configuration in another location, it can be passed as an argument to lsmd. Next, add the following lines:
```
[lsm]
addr=127.0.0.1
port=42024
```
42024 is the default port for the lsm client, but any port can be used with additional configuration. Notice that here, lines in brackets denote service names, and any lines until the next bracket denote options for the antecedent service.

Now say we want to run an mpd server from lsm:
```
[mpd]
start=mpd --no-daemon
```

We can also specify dependencies for services. When lsm starts them, it will try to start the dependencies too:
```
[wireplumber]
start=wireplumber
require=pipewire
```
lsm accepts additional config lines but for the sake of brevity I omit them here.

# Running
lsmd is the process supervisor and lsm is the default client. lsmd accepts one argument, a configuration location. lsm accepts directives as described by the "help" argument. For example, `lsm start mpd` would run `mpd --no-daemon`.
