import EmployeeValidator._
import cats.data.NonEmptyList
import cats.data.Xor.{Left, Right}
import monocle.macros.GenLens
import org.specs2._
import org.zalando.jsonapi.model.JsonApiObject.{NumberValue, StringValue}
import org.zalando.jsonapi.model.Links.{Related, Self}
import org.zalando.jsonapi.model.RootObject.{Data, ResourceObject, ResourceObjects}
import org.zalando.jsonapi.model.{Attribute, RootObject}

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

  import monocle.std._

  val _data            = GenLens[RootObject](_.data)
  val _resourceObjects = GenLens[Data](_.resourceObjects)
/*
  val _resourceObject  = Lens[ResourceObjects, ResourceObject](???)(???)
  val _attribute       = Lens[ResourceObject, Attribute](???)(???)*/

  "RootObjectValidator spec" >> {

    "returns error if RootObject is empty" >> {
      val root = rootObject.copy(data = None, links = None)
      validator.validate(root) mustEqual Left(NonEmptyList(EmptyRootObject))
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
