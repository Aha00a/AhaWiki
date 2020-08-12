package logics.wikis.interpreters

import org.scalatest.freespec.AnyFreeSpec

import scala.xml.Elem

class TraitInterpreterSpec extends AnyFreeSpec {

  import logics.wikis.interpreters.ahaMark.AhaMark


  case class AhaMarkDocument(seq:Seq[AhaMark]) extends AhaMark {
    override def toHtml: Elem = <div>{seq.map(_.toHtml)}</div>
  }

  case class AhaMarkHeading(depth: Int, name: String, id: String = "") extends AhaMark {
    override def toHtml: Elem = <h1 id={id}>{name}</h1>.copy(label = "h" + depth)
  }

  case class AhaMarkLink(uri: String, alias: String) extends AhaMark {
    override def toHtml: Elem = <a href={uri}>{alias}</a>
  }


  "test" in {
    assert(AhaMarkDocument(Seq(AhaMarkHeading(1, "AhaMark", "id"))).toHtml.toString === """<div><h1 id="id">AhaMark</h1></div>""")
    assert(AhaMarkDocument(Seq(AhaMarkHeading(1, "AhaMark", "id"))).toText === """AhaMark""")
  }
}
