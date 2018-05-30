package cats
package tests

import cats.data.Store
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._

class StoreTSuite extends CatsSuite {
  checkAll("Store[Int, String]", ComonadTests[Store[Int, ?]].comonad[String, String, String])
}