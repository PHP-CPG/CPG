<?php
function first_function($param1, $param2) {
    echo $param1 . $param2;
}

function second_function($param1, $param2 = 42) {
    echo $param1 . $param2;
}

first_function(1,2);
second_function(2);
?>