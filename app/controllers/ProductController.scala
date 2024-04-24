package controllers

import javax.inject._
import play.api.mvc._
import play.api.libs.json._

case class Product(id: Long, name: String, price: Double)

@Singleton
class ProductController @Inject()(val controllerComponents: ControllerComponents) extends BaseController {

  var products: List[Product] = List(
    Product(1, "Carrot", 2.50),
    Product(2, "Apple", 2.99),
    Product(3, "Banana", 7.79),
    Product(4, "Tomato", 9.80)
  )

  implicit val productReads: Reads[Product] = Json.reads[Product]
  implicit val productWrites: Writes[Product] = Json.writes[Product]

  def getAllProducts: Action[AnyContent] = Action {
    Ok(Json.toJson(products))
  }

  def getProductById(id: Long): Action[AnyContent] = Action {
    products.find(_.id == id) match {
      case Some(product) => Ok(Json.toJson(product))
      case None => NotFound("Product not found")
    }
  }

  def addProduct: Action[JsValue] = Action(parse.json) { request =>
    request.body.validate[Product].fold(
      errors => {
        BadRequest(Json.obj("message" -> JsError.toJson(errors)))
      },
      product => {
        products = products :+ product
        Created(Json.toJson(product))
      }
    )
  }

  def updateProduct(id: Long): Action[JsValue] = Action(parse.json) { request =>
    request.body.validate[Product].fold(
      errors => {
        BadRequest(Json.obj("message" -> JsError.toJson(errors)))
      },
      updatedProduct => {
        products.find(_.id == id) match {
          case Some(_) =>
            products = products.map(p => if (p.id == id) updatedProduct else p)
            Ok(Json.toJson(updatedProduct))
          case None =>
            NotFound("Product not found")
        }
      }
    )
  }

  def deleteProduct(id: Long): Action[AnyContent] = Action {
    products.find(_.id == id) match {
      case Some(_) =>
        products = products.filterNot(_.id == id)
        Ok("Product deleted")
      case None =>
        NotFound("Product not found")
    }
  }
}
