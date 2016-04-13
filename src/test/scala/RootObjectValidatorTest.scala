import EmployeeValidator._
import cats.data.NonEmptyList
import cats.data.Xor.{Left, Right}
import monocle.{Iso, Lens, POptional, Prism}
import monocle.macros.{GenIso, GenLens, GenPrism}
import org.specs2._
import org.zalando.jsonapi.model.JsonApiObject.{NumberValue, StringValue}
import org.zalando.jsonapi.model.Links.{Related, Self}
import org.zalando.jsonapi.model.RootObject.{Data, ResourceObject, ResourceObjects}
import org.zalando.jsonapi.model.{Attribute, RootObject}

import scala.collection.immutable.Seq

class RootObjectValidatorTest extends mutable.Specification {

  val validator = new EmployeeValidator

  val rootObject = RootObject(
    data = Some {
      ResourceObjects {
        List {
          ResourceObject(
            `type` = "employee",
            id = Some("id-1"),
            attributes = Some {
              List(
                Attribute(name = "name", value = StringValue("John Doe")),
                Attribute(name = "email", value = StringValue("John.Doe@zalando.de")),
                Attribute(name = "salary", value = NumberValue(3000))
              )
            }
          )
        }
      }
    },
    links = Some {
      List(
        Self(url = "http://example.com/resource1/id-1"),
        Related(url = "http://example.com/resource-related/id-2")
      )
    }
  )

  import monocle.std.option.{some ⇒ SomePrism}
  import monocle.function.Cons._
  import monocle.std.list._
  import monocle._

  def seqToList[A] = Iso[Seq[A], List[A]](_.toList)(_.seq)
  val _data = GenLens[RootObject](_.data) composePrism SomePrism
  val _resourceObjects = _data composePrism GenPrism[Data, ResourceObjects]
  val _resourcesObjectsList = _resourceObjects composeIso GenIso[ResourceObjects, Seq[ResourceObject]] composeIso seqToList
  val _resource = _data composePrism GenPrism[Data, ResourceObject]
  val _firstResource = _resourcesObjectsList composeOptional headOption
  val _attributes = GenLens[ResourceObject](_.attributes) composePrism SomePrism composeIso seqToList

  "RootObjectValidator spec" >> {

    "returns error if RootObject is empty" >> {
      val root = rootObject.copy(data = None, links = None)
      validator.validate(root) mustEqual Left(NonEmptyList(EmptyRootObject))
    }

    "returns error if RootObject has no attributes" >> {
      val rootWithoutAttributes = (_firstResource composeOptional _attributes).set(Nil)(rootObject)
      validator.validate(rootWithoutAttributes) mustEqual Left(NonEmptyList(RequiredAttributeIsMissing("salary")))
    }

    "returns error if salary is negative" >> {
      val root = rootObject
      validator.validate(root) mustEqual Left(NonEmptyList(NegativeSalary))
    }

    "returns valid RootObject" >> {
      validator.validate(rootObject) mustEqual Right(rootObject)
    }

  }

}
