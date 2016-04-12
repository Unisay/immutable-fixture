import cats.data.{NonEmptyList, Xor}
import cats.data.Xor.{Left, Right}
import org.zalando.jsonapi.model.JsonApiObject.NumberValue
import org.zalando.jsonapi.model.RootObject
import org.zalando.jsonapi.model.RootObject.{ResourceObject, ResourceObjects}

object EmployeeValidator {
  sealed trait Error
  case object EmptyRootObject extends Error
  case object NegativeSalary extends Error
  case class RequiredAttributeIsMissing(attribute: String) extends Error

  type ValidatedEmployee = NonEmptyList[Error] Xor RootObject
}

class EmployeeValidator {
  import EmployeeValidator._

  def validate(employee: RootObject): ValidatedEmployee = employee match {
    case RootObject(None, None, None, None, None, None) ⇒
      Left(NonEmptyList(EmptyRootObject))
    case RootObject(Some(ResourceObjects(Seq(ResourceObject(_, _, Some(attributes), _, _, _)))), _, _, _, _, _)
      if attributes.exists { attribute ⇒
        attribute.name == "salary" &&
        attribute.value.isInstanceOf[NumberValue] &&
        attribute.value.asInstanceOf[NumberValue].value < 0
      } ⇒
      Left(NonEmptyList(NegativeSalary))
    case _ ⇒
      Right(employee)
  }

}
