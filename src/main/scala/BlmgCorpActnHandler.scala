import org.nirvana._
import org.joda.time.{Period, DateTime, Duration}
import org.joda.time.format.DateTimeFormat

trait CorpActionType { val symbol: String }
case class Delisting(symbol: String, effectiveDate: DateTime) extends CorpActionType
case class TickerSymbolChange(symbol: String, toSymbol: String, effectiveDate: DateTime) extends CorpActionType
case class CashDividend(symbol: String, effectiveDate: DateTime, dividend: Double) extends CorpActionType
case class StockDividend(symbol: String, effectiveDate: DateTime, adjustmentFactor: Double) extends CorpActionType
case class StockSplit(symbol: String, effectiveDate: DateTime, adjustmentFactor: Double) extends CorpActionType
case class IgnoredCorpAction(symbol: String) extends CorpActionType

trait InputDateFmt
case class InputDateFmt_YMD extends InputDateFmt
case class InputDateFmt_DMY extends InputDateFmt

object BlmgCorpActnHandler {
  var inputDateFormat: InputDateFmt = InputDateFmt_YMD()
  val dfmt = DateTimeFormat.forPattern("yyyy-MM-dd")
  val STOCK_SPLIT = "Stock Split"
  val CASH_DIVIDEND = "Cash Dividend"
  val STOCK_DIVIDEND = "Stock Dividend"
  val DELISTING = "Delisting"
  val TICKER_SYMBOL_CHANGE = "Ticker Symbol Change"

  val NET_AMOUNT = "Net Amount"
  val ADJ_FACTOR = "Adjustment Factor"
  val OTHER_SYMBOL = "Other Symbol"

  def getFieldValue(fields: List[String], fieldName: String): (String, DateTime, Option[Double]) = {
    val symbol = fields(1).split(" ").head
    val lsEffDate = fields(3).split("/")
    val effDate = inputDateFormat match {
      case InputDateFmt_DMY() => new DateTime(lsEffDate(2).toInt, lsEffDate(1).toInt, lsEffDate(0).toInt, 0, 0, 0)
      case InputDateFmt_YMD() => new DateTime(lsEffDate(0).toInt, lsEffDate(1).toInt, lsEffDate(2).toInt, 0, 0, 0)
      case _                  => new DateTime(lsEffDate(0).toInt, lsEffDate(1).toInt, lsEffDate(2).toInt, 0, 0, 0)
    }
    val fieldVal = fields.tail.filter(_.contains(fieldName)).map(field => field.split(":").toList.map(_.trim).last).head

    (symbol, effDate, SUtil.safeToDouble(fieldVal))
  }

  def main(args: Array[String]) = {

    if (args.length == 0) {
      println("USAGE: [bloomberg cad file] [ymd|dmy]")
      System.exit(0)
    }

    inputDateFormat = if (args(1) == "ymd") InputDateFmt_YMD() else InputDateFmt_DMY()

    val lslsRawAction = scala.io.Source.fromFile(args(0)).getLines.toList.
      filter(!_.contains("Bloomberg")).
      filter(!_.contains("Action Type")).
      map(_.split("\t").toList)

    val lsCorpAction = lslsRawAction.map(actionFields => {
      if (actionFields.length > 2) {
        if (actionFields(0).contains(CASH_DIVIDEND)) {

          val (symbol, effDate, optDbl) = getFieldValue(actionFields, NET_AMOUNT)

          SUtil.safeToInt(symbol) match {
            case Some(i) => {
              optDbl match {
                case Some(d) => CashDividend(symbol, effDate, d)
                case None    => IgnoredCorpAction(symbol)
              }
            }
            case None => IgnoredCorpAction(OTHER_SYMBOL)
          }
        }
        else if (actionFields(0).contains(STOCK_SPLIT)) {

          val (symbol, effDate, optDbl) = getFieldValue(actionFields, ADJ_FACTOR)

          SUtil.safeToInt(symbol) match {
            case Some(i) => {
              optDbl match {
                case Some(d) => StockSplit(symbol, effDate, d)
                case None    => IgnoredCorpAction(symbol)
              }
            }
            case None => IgnoredCorpAction(OTHER_SYMBOL)
          }
        }
        else {
          IgnoredCorpAction(OTHER_SYMBOL)
        }
      }
    })

    lsCorpAction.foreach(a => a match {
      // case CashDividend(s, edt, d) => println(dfmt.print(edt) + "," + SUtil.addLeadingChar(s, '0', 5) + "," + d.toDouble + ",1")
      case StockSplit(s, edt, d) => println(dfmt.print(edt) + "," + SUtil.addLeadingChar(s, '0', 5) + ",0," + d.toDouble)
      case _ => Unit
    })

  }
}
