<?php
class a
{
    function foo()
    {
        echo 1;
    }
}

class b
{
    function __construct($a)
    {
        $a->foo();
    }
}
