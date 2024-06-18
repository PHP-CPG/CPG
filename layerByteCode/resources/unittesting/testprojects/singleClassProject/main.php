 <?php
 class Basic {

     public $var = 42;

     function __construct($val) {
         $var = $val;
     }

     private function test($test) {
         echo $test;
     }

     public function test2($test) {
         $this->test($rest);
         test($rest);
     }
 }

 $var = new Basic("value");
 $var->test2("other");
 ?>