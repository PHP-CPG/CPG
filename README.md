# php-cpg
A (Bytecode) CPG creator for PHP

# Status
![Scala CI](https://github.com/simkoc/php-cpg/workflows/Scala%20CI/badge.svg)

# Dependencies

- [patched](https://github.com/simkoc/php-src) PHP
  - PHP 7.4.27 from branch [`PHP-7.4.27`](https://github.com/simkoc/php-src/tree/PHP-7.4.27)
  - PHP 8.2.2 from branch [`PHP-8.2.2`](https://github.com/simkoc/php-src/tree/PHP-8.2.2)
  - we recommend to build them locally for dev purposes
  - consider using the Dockerfiles in resources/docker for productive use
- sbt
- scala
- php-parse (only for the source code layer)

# Building and Running

## Dependencies

You need to download and install our [patched](https://github.com/simkoc/php-src/) PHP versions. Use the branches as specified above.

Create the `main.conf` by using the main.conf.default and adjust the parameters according to your system.

## Building the project
The project can be build and run with the subsequent steps, yet it will only work with the `bytecode` argument. The source code support relies on `php-parse`, you can find the install instructions in the sub section `Source code support`. 
```
sbt stage
./php2cpg -h
no more arguments and a subaction has still to be chosen

usage: PHP Cpg Creator [rootFolder] {-c,-o,-e,-h,-f,-s,-l,-v} log-evaluation bytecode

create a cpg from PHP

          [rootFolder] the root folder of the project

          -c/--config <value> the config file to use (def:./main.conf)

          -o/--output <value> the destination file into which to store the cpg (def:<config>) 

          -e/--endings <value> the file endings included for analysis (def:<config>) 

          -h/--help prints this help message

          -f/--forced when set files are simply overwritten

          -s/--strictParsing if set each project file has to be parsed successfully

          -l/--permissiveLinking linking strategy for call graph set to permissive

          -v/--verbose output some progress indicators

          log-evaluation 

          bytecode create a bytecode CPG
```

## Source code support
Source code is analyzed using `php-parse`. The following install steps are taken from the official install instructions: https://getcomposer.org/download/.
```
php -r "copy('https://getcomposer.org/installer', 'composer-setup.php');" 
php -r "if (hash_file('sha384', 'composer-setup.php') === '795f976fe0ebd8b75f26a6dd68f78fd3453ce79f32ecb33e7fd087d39bfeb978342fb73ac986cd4f54edd0dc902601dc') { echo 'Installer verified'; } else { echo 'Installer corrupt'; unlink('composer-setup.php'); } echo PHP_EOL;"
php composer-setup.php 
php -r "unlink('composer-setup.php');"

# Location is expected by php2cpg 
mkdir -p $HOME/.config/composer/vendor/ && COMPOSER_VENDOR_DIR=$HOME/.config/composer/vendor/ php composer.phar require nikic/php-parser
```


# Documentation

[Opcodes](./documentation/README.md)

# Using Docker

Build the image
```
docker build -t phpcpg /your/path/to/php-cpg/ --build-arg GH_TOKEN=<your token>
```

Run the image
```
➜  ~ docker run -it phpcpg /bin/bash
root@f43557f5be46:/php-cpg# ./php2cpg 
The first parameter has to be <sourcecode|bytecode> to determine which cpg layer to create
usage:
./php2cpg sourcecode main.php
./php2cpg bytecode main.php
```

## Token

Create the github token at https://github.com/settings/tokens. The token needs to have `repo` permissions.

