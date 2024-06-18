package io.joern.reporting

import java.time.LocalDateTime

case class PassWarning(file: String,
                       line: String,
                       method: String,
                       bytecode: String,
                       description: String) {

  override def toString: String = {
    s"{$file:$line}{$method}{$bytecode} $description"
  }

}

case class PassError(file: String,
                     line: String,
                     method: String,
                     bytecode: String,
                     description: String,
                     stacktrace: String) {
  override def toString: String = {
    s"{$file:$line}{$method}{$bytecode} $description \n $stacktrace"
  }
}

case class PassReport(name: String,
                      errors: Seq[PassError],
                      warnings: Seq[PassWarning],
                      start: LocalDateTime,
                      end: LocalDateTime = LocalDateTime.now())
