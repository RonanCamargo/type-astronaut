package ronancamargo.json

import ronancamargo.json.ast.JsonObject

trait JsonObjectEncoder[A] extends JsonEncoder[A] {
  def encode(value: A): JsonObject
}
