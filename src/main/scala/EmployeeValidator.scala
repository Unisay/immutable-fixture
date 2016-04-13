import cats.data.{NonEmptyList, Xor}
import cats.data.Xor.{Left, Right}
import org.zalando.jsonapi.model.JsonApiObject.{NumberValue, Value}
import org.zalando.jsonapi.model.{JsonApiObject, RootObject}
import org.zalando.jsonapi.model.RootObject.{ResourceObject, ResourceObjects}

object EmployeeValidator {
  sealed trait Error
  case object EmptyRootObject extends Error
  case object NegativeSalary extends Error
  case class RequiredAttributeIsMissing(attribute: String) extends Error
  case class WrongAttributeType[T <: JsonApiObject.Value](attribute: String) extends Error

  type ValidatedEmployee = NonEmptyList[Error] Xor RootObject
}

class EmployeeValidator {
  import EmployeeValidator._

  def validate(employee: RootObject): ValidatedEmployee = employee match {
    case RootObject(None, None, None, None, None, None) ⇒
      Left(NonEmptyList(EmptyRootObject))

    case RootObject(Some(ResourceObjects(Seq(ResourceObject(_, _, Some(attributes), _, _, _)))), _, _, _, _, _)
      if !attributes.exists(_.name == "salary") ⇒
      Left(NonEmptyList(RequiredAttributeIsMissing("salary")))
    case RootObject(Some(ResourceObjects(Seq(ResourceObject(_, _, Some(attributes), _, _, _)))), _, _, _, _, _)
      if !attributes.exists(a ⇒ a.name == "salary" && a.value.isInstanceOf[NumberValue]) ⇒
      Left(NonEmptyList(WrongAttributeType[NumberValue]("salary")))
    case RootObject(Some(ResourceObjects(Seq(ResourceObject(_, _, Some(attributes), _, _, _)))), _, _, _, _, _)
      if attributes.exists(a ⇒ a.name == "salary" && a.value.asInstanceOf[NumberValue].value < 0) ⇒
      Left(NonEmptyList(NegativeSalary))

    case _ ⇒
      Right(employee)
  }

}
