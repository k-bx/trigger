# trigger

Kill and restart the process when the executable changes

```
$ trigger /path/to/webserver/or/something
```

Works great with your stack web apps:

```
$ trigger $(stack exec -- which YourApp)
```

## Installation

Install via `stack install`
