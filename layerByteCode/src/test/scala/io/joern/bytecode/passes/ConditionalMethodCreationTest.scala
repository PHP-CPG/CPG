package io.joern.bytecode.passes

import io.joern.bytecode.parser
import io.joern.bytecode.parser.PHPVersions
import io.joern.bytecode.util.unittesting.CpgFromCodeTestFixture
import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ConditionalMethodCreationTest
    extends AnyWordSpec
    with Matchers  with PHPVersions {

  for(v <- getPhpVersions) {
    implicit val version: parser.PHPVersion.Value = v

    s"cpg for PHP $version" should {
      "contain two imce_image_info methods" in new CpgFromCodeTestFixture(
        """if (variable_get('imce_image_get_info', 0)) {
          |    function imce_image_info($file) {
          |        $mimes = array('image/jpeg' => IMAGETYPE_JPEG, 'image/gif'  => IMAGETYPE_GIF, 'image/png'  => IMAGETYPE_PNG);
          |        if (is_file($file) && ($dot = strrpos($file, '.')) && in_array(strtolower(substr($file, $dot+1)), array('jpg', 'jpeg', 'gif', 'png')) && ($info = @image_get_info($file)) && isset($mimes[$info['mime_type']]) ) {
          |          return array('width' => $info['width'], 'height' => $info['height'], 'type' => $mimes[$info['mime_type']], 'mime' => $info['mime_type']);
          |        }
          |        return FALSE;
          |      }
          |    }
          |    else {
          |      function imce_image_info($file) {
          |      if (is_file($file) && ($dot = strrpos($file, '.')) && in_array(strtolower(substr($file, $dot+1)), array('jpg', 'jpeg', 'gif', 'png')) && ($info = @getimagesize($file)) && in_array($info[2], array(IMAGETYPE_JPEG, IMAGETYPE_GIF, IMAGETYPE_PNG)) ) {
          |        return array('width' => $info[0], 'height' => $info[1], 'type' => $info[2], 'mime' => $info['mime']);
          |      }
          |    return FALSE;
          |   }
          |}
          |""".stripMargin) {
        cpg.method("imce_image_info").toList.length shouldBe 2
      }
      "poc for rc" in new CpgFromCodeTestFixture(
        """if (variable_get('imce_image_get_info', 0)) {
          |    function c($file) {
          |        b();
          |        a();
          |      }
          |    }
          |    else {
          |      function c($file) {
          |           b();
          |           a();
          |   }
          |}
          |""".stripMargin) {
        // no crash means passing
      }
      "work with two equally named conditional functions" in new CpgFromCodeTestFixture(
        """if($cond) {
          |   function test($var) {
          |     funcall();
          |   }
          |} else {
          |   function test($var) {
          |     funcall();
          |   }
          |}
          |""".stripMargin
      )
    }
  }
}
