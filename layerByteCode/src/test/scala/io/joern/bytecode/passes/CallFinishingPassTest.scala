package io.joern.bytecode.passes

import io.joern.bytecode.parser.PHPVersions
import io.joern.bytecode.passes.utility.MethodDetectionAndAssociation
import io.joern.bytecode.util.implicits.OneableSeq
import io.joern.bytecode.util.unittesting.CpgFromCodeTestFixture
import io.joern.bytecode.{Defines, parser}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Method}
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, nodes}
import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable
import scala.jdk.CollectionConverters._

class CallFinishingPassTest extends AnyWordSpec with Matchers with PHPVersions {

  /*"compangnion object function getPhpInternalFucntions" should {
    "return a set greater zero" in {
      CallFinishingPass.getPhpInternalFunctions.nonEmpty shouldBe true
    }
    "contain get_defined_constants" in {
      CallFinishingPass.getPhpInternalFunctions.contains(
        "get_defined_constants") shouldBe true
    }
    "contains mysql_error" in {
      CallFinishingPass.getPhpInternalFunctions.contains("pclose") shouldBe true
    }
  }*/

  for (v <- getPhpVersions) {
    implicit val version: parser.PHPVersion.Value = v

    s"call finishing pass for PHP $version" should {

      "have the called function argument node as well as two additional arguments" in new CpgFromCodeTestFixture(
        """
          |$test = mysqli_query($link,$query);
          |""".stripMargin
      ) {
        val call: Call = cpg.call.nameExact("mysqli_query").l.head
        call.argumentOut.l.map(_.code).toSet shouldBe Set("SEND_VAR_EX CV($link) int(1)", "SEND_VAR_EX CV($query) int(2)")
        val calling: Seq[Method] = call.out(EdgeTypes.CALL).asScala.toList.map(_.asInstanceOf[Method])
        calling.length shouldBe 1
        calling.head.code shouldBe Defines.UNKNOWN_FUNCTION
      }

      "work even if there is a exit branch inside a call" in new CpgFromCodeTestFixture(
        """
          |$test = mysqli_query($link, $cond ? $query : exit());
          |""".stripMargin
      ) {
        val call: List[Call] = cpg.call.nameExact("mysqli_query").l
        call.length shouldBe 1
        call.head.argumentOut.l.map(_.code).toSet shouldBe Set("SEND_VAR_EX CV($link) int(1)", "SEND_VAL_EX T4 int(2)")
        val calling: Seq[Method] = call.head.out(EdgeTypes.CALL).asScala.toList.map(_.asInstanceOf[Method])
        calling.length shouldBe 1
        calling.head.code shouldBe Defines.UNKNOWN_FUNCTION
      }

      "be able to handle nested calls" in new CpgFromCodeTestFixture(
        """
          |outer(inner($first),inner2($second));
          |""".stripMargin
      ) {
        val outer: Call = cpg.call.nameExact("outer").l.head
        val inner: Call = cpg.call.nameExact("inner").l.head
        val inner2: Call = cpg.call.nameExact("inner2").l.head
        inner.argumentOut.l.map(_.code).toSet shouldBe Set("SEND_VAR_EX CV($first) int(1)")
        inner2.argumentOut.l.map(_.code).toSet shouldBe Set("SEND_VAR_EX CV($second) int(1)")
        outer.argumentOut.l.map(_.code).toSet shouldBe Set("SEND_VAR_NO_REF_EX V2 int(1)", "SEND_VAR_NO_REF_EX V3 int(2)")
      }

      "be able to handle self defined functions" in new CpgFromCodeTestFixture(
        """
          |function test() {
          | echo "test";
          |}
          |
          |test();
          |""".stripMargin
      ) {
        val call: Call = cpg.call.nameExact("test").l.head
        call.argumentOut.l.map(_.code).toSet shouldBe Set()
      }

      "create the correct call even though it is a subcall" in new CpgFromCodeTestFixture(
        """
          |echo $e->errorMessage();
          |""".stripMargin
      ) {
        cpg.call.nameExact(".*::errormessage").l.length shouldBe 1
      }

      "have the all the correct names" in new CpgFromCodeTestFixture(
        """try {
          |
          |} catch (phpmailerException $e) {
          |  echo $e->errorMessage(); //Pretty error messages from PHPMailer
          |}
          |""".stripMargin
      ) {
        cpg.call.filter(_.out(EdgeTypes.CALL).hasNext).map(_.name).toSet shouldBe Set(".*::errormessage")
      }

      "be able to handle a call within a return" in new CpgFromCodeTestFixture(
        """
          |function getImageBuffer($image) {
          |    if ($this->diskcache AND isset($this->images[$image])) {
          |        return unserialize($this->readDiskCache($this->images[$image]));
          |    } elseif (isset($this->images[$image])) {
          |        return $this->images[$image];
          |    }
          |    return false;
          |}
          |""".stripMargin
      ) {
        val calls: mutable.Set[String] = cpg.call.filter(_.out(EdgeTypes.CALL).hasNext).map(_.name).toSet
        calls shouldBe Set("unserialize", ".*::readdiskcache")
      }

      "be to handle special drupal case I" in new CpgFromCodeTestFixture(
        """function ValidateAddress($address) {
          |   if (function_exists('filter_var')) { //Introduced in PHP 5.2
          |      if(filter_var($address, FILTER_VALIDATE_EMAIL) === FALSE) {
          |         return false;
          |      } else {
          |         return true;
          |      }
          |   } else {
          |       return preg_match('regexpString', $address);
          |   }
          |}
          |""".stripMargin
      ) {
        val calls: mutable.Set[String] = cpg.call.filter(_.out(EdgeTypes.CALL).hasNext).map(_.name).toSet
        calls shouldBe Set("function_exists", "filter_var", "preg_match")
      }

      "be able to handle special drupal case II" in new CpgFromCodeTestFixture(
        """
          |class Klasse {
          | public function Send() {
          |    try {
          |      switch($this->Mailer) {
          |        default:
          |          return $this->MailSend($header, $body);
          |      }
          |    } catch (phpmailerException $e) {
          |      echo $e->getMessage()."\n";
          |    }
          |  }
          |}
          |""".stripMargin
      ) {
        val calls: mutable.Set[String] = cpg.call.filter(_.out(EdgeTypes.CALL).hasNext).map(_.name).toSet
        calls shouldBe Set("klasse::mailsend", ".*::getmessage")
      }

      "be able to handle drupal case III" in new CpgFromCodeTestFixture(
        """function add($name, $title = '', $body = '') {
          |    if (is_object($name) && is_subclass_of($name, 'views_tab')) {
          |      $this->add_tab($name);
          |    }
          |    elseif (is_array($name)) {
          |      foreach ($name as $real_tab) {
          |        $this->add($real_tab);
          |      }
          |    }
          |    else {
          |      $this->add_tab(new views_tab($name, $title, $body));
          |    }
          |  }
          |""".stripMargin
      ) {
        val calls: mutable.Set[String] = cpg.call.map(_.name).toSet
      }
      "be able to correctly handle SEND_ARRAY" in new CpgFromCodeTestFixture(
        """
          |call_user_func_array($field["callback"], array($field));
          |""".stripMargin
      ) {
        cpg.call.code("DO_FCALL").next()
        val sendArray: Call = cpg.call("SEND_ARRAY").next()
        val argOfSendArray: nodes.CfgNode = sendArray.astChildren.order(MethodDetectionAndAssociation.getSendValuePos(sendArray)).next().asInstanceOf[nodes.CfgNode]
        argOfSendArray.code shouldBe "T2"

      }
      "be able to utilize this marker to distinguish between equal function names" in new CpgFromCodeTestFixture(
        """class A {
          |   function bob() {
          |       echo "do stuff";
          |   }
          |
          |   function callBob() {
          |       $this->bob();
          |   }
          |}
          |
          |class B {
          |   function bob() {
          |       echo "do other stuff";
          |   }
          |   function callBob() {
          |       $this->bob();
          |   }
          |}
          |""".stripMargin) {
        cpg.call.code("DO_FCALL").foreach {
          node: nodes.Call =>
            if (!Set("a::bob", "b::bob").contains(node.name)) {
              fail(s"call node name ${node.name} is unexpected")
              //node.out(EdgeTypes.CALL).asScala.toList.length shouldBe 1
            }
        }
      }
      "be able to call link static method calls" in new CpgFromCodeTestFixture(
        """class A {
          |   public static function stafu() {
          |       echo "dostuff";
          |   }
          |}
          |A::stafu();
          |""".stripMargin) {
        val call: Call = cpg.call.code("DO_UCALL").next()
        call.name shouldBe "a::stafu"
        call.out(EdgeTypes.CALL).asScala.toList.length shouldBe 1
      }
      "be able to set the name for self::randomToken() in class" in new CpgFromCodeTestFixture(
        """class c
          |{
          |    public static function token()
          |    {
          |        return self::randomToken();
          |    }
          |}
          |""".stripMargin) {
        private val call = cpg.call.filter(_.code.startsWith("DO_")).l.one
        call.name shouldBe "c::randomtoken"
      }
      "not set multiple call edges (none in this case)" in new CpgFromCodeTestFixture(
        """interface i {
          |   public function foo($param);
          |}
          |
          |class Klasse implements i {
          |   public function foo($param) {
          |     echo $param;
          |   }
          |}
          |$obj = new Klasse();
          |$obj->foo(42);
          |""".stripMargin) {
        val calls: mutable.Set[String] = cpg.call.filter(_.out(EdgeTypes.CALL).hasNext).map(_.name).toSet
        calls shouldBe Set(".*::foo", "klasse::__construct")
        val call: Seq[Call] = cpg.call.nameExact(".*::foo").l
        call.length shouldBe 1
        call.head.out(EdgeTypes.CALL).asScala.toList.length shouldBe 1
      }
      "correctly create a internal function dummy" in new CpgFromCodeTestFixture(
        """
          |str_replace("test","test","test");
          |""".stripMargin
      ) {
        val calls: mutable.Set[String] = cpg.call.filter(_.out(EdgeTypes.CALL).hasNext).map(_.name).toSet
        calls shouldBe Set("str_replace")
        val methods: Seq[Method] = cpg.method.nameExact("str_replace").l
        methods.length shouldBe 1
        methods.head.code shouldBe Defines.INTERNAL_FUNCTION
      }
      "handle function name collisions gracefully" when {
        "collision in name" in new CpgTestFixture("nameCollision") {
          cpg.method.fullName.l.toSet shouldEqual Set("a::foo", "a::__construct", "b::__construct", "dlr_main", "UNKNOWN::foo")
          cpg.method.fullName("a::foo").size shouldBe 2
          cpg.method("a::__construct").l.one.code shouldBe Defines.CONSTRUCTOR
        }
        "collision via namespace and other stuff" in new CpgTestFixture("guzzleunknown") {
          assert(cpg.method.nonEmpty) // for now: just dont fail
        }
        "function defined twice" in new CpgTestFixture("functionDefinedTwice") {
          assert(cpg.method.l.nonEmpty)
        }
      }
      "be able to handle namespace covering up one internal function but not the second" in new CpgTestFixture("twoInternalFunctionsOneNamespace") {
        cpg.method.filter(_.code == Defines.INTERNAL_FUNCTION).l.length shouldBe 1
      }
      "be able to ignore namespace if function in namespace does not exist but basic function outside does" in new CpgFromCodeTestFixture(
        """namespace test;
          |
          |sin(1);
          |
          |""".stripMargin
      ){
        val methods: Seq[Method] = cpg.method.code(Defines.INTERNAL_FUNCTION).l
        methods.length shouldBe 1
        methods.head.name shouldBe "sin"
        methods.head.in(EdgeTypes.CALL).hasNext shouldBe true
      }
      "be able to handle function defines that overwrite internal functions" in new CpgFromCodeTestFixture("""namespace {
                                                                                                             |	if (!is_callable('random_bytes')) {
                                                                                                             |		function random_bytes($length)
                                                                                                             |		{
                                                                                                             |			return 1;
                                                                                                             |		}
                                                                                                             |	}
                                                                                                             |}
                                                                                                             |
                                                                                                             |namespace KeePassPHP {
                                                                                                             |
                                                                                                             |	echo random_bytes(32);
                                                                                                             |}""".stripMargin){
        cpg.method.size should be > 0
      }
    }
  }
}
