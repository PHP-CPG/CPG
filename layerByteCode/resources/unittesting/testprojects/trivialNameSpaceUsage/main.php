<?php

namespace weird\testOne;

function notAMethod() {
    echo("test");
}

class test {

    function test() {
        echo("test");
    }

}

namespace testTwo;

function notAMethod() {
    echo("test");
}

class test {

    function test() {
        echo("test");
    }

}

use weird\testOne;

testOne\notAMethod();
notAMethod();

?>