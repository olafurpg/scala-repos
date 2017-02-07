package lila.donation

import play.api.data._
import play.api.data.Forms._

object DataForm {

  private val txnTypes = Set("express_checkout",
                             "web_accept",
                             "recurring_payment",
                             "subscr_payment")

  val ipn = Form(
    mapping(
      "txn_id" -> optional(nonEmptyText),
      "subscr_id" -> optional(nonEmptyText),
      "txn_type" -> nonEmptyText.verifying("Invalid txn type",
                                           txnTypes contains _),
      "mc_gross" -> bigDecimal,
      "mc_fee" -> bigDecimal,
      "custom" -> optional(text),
      "payer_email" -> optional(nonEmptyText),
      "first_name" -> optional(nonEmptyText),
      "last_name" -> optional(nonEmptyText)
    )(Ipn.apply)(Ipn.unapply))

  case class Ipn(txnId: Option[String],
                 subId: Option[String],
                 txnType: String,
                 gross: BigDecimal,
                 fee: BigDecimal,
                 userId: Option[String],
                 email: Option[String],
                 firstName: Option[String],
                 lastName: Option[String]) {

    def name = ((firstName |@| lastName)).apply { _ + " " + _ }

    def grossCents = (gross * 100).toInt

    def feeCents = (fee * 100).toInt
  }
}
