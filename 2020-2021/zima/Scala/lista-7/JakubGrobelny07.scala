package object money {

    sealed trait Currency { def toString(i: BigDecimal): String }
    case object USD extends Currency { def toString(i: BigDecimal) = "$" + i   }
    case object EUR extends Currency { def toString(i: BigDecimal) = "€" + i   }
    case object PLN extends Currency { def toString(i: BigDecimal) = i + " zł" }

    def $   = USD
    def zł  = PLN
    def `€` = EUR

    implicit class BigDecimalToMoney(amount: BigDecimal) {
        def apply(currency: Currency)(implicit converter: CurrencyConverter) = Money(amount, currency)
    }

    implicit class IntToMoney(amount: Int) {
        def apply(currency: Currency)(implicit converter: CurrencyConverter) = BigDecimal(amount)(currency)
    }

    implicit class DoubleToMoney(amount: Double){
        def apply(currency: Currency)(implicit converter: CurrencyConverter) = BigDecimal(amount)(currency)
    }

    case class Money (val amount: BigDecimal, val currency: Currency)(implicit converter: CurrencyConverter) {
        def +(other: Money) = Money(amount + (other as currency).amount, currency)

        def unary_-() = Money(-amount, currency)

        def -(other: Money) = this + -other

        def *(n: BigDecimal) = Money(n * amount, currency)

        def as(targetCurrency: Currency) = {
            val newAmount = amount * converter.exchangeRate(currency, targetCurrency)
            Money(newAmount, targetCurrency)
        }

        private def isValueInRelation(other: Money, rel: (BigDecimal, BigDecimal) => Boolean): Boolean =
            rel(this.amount, (other as currency).amount)

        def ==(other: Money): Boolean = isValueInRelation(other, _ == _)

        def !=(other: Money): Boolean = isValueInRelation(other, _ != _)

        def <=(other: Money): Boolean = isValueInRelation(other, _ <= _)

        def >=(other: Money): Boolean = isValueInRelation(other, _ >= _)

        def <(other: Money): Boolean  = isValueInRelation(other, _ < _)

        def >(other: Money): Boolean  = isValueInRelation(other, _ > _)

        override def toString() = currency.toString(amount)
    }


    trait CurrencyConverter {
        def exchangeRate(from: Currency, to: Currency): BigDecimal
    }
}

object Example {
    import money._

    object CurrencyConverterMock extends CurrencyConverter {
        val conversion: Map[(Currency, Currency), BigDecimal] = Map(
            (EUR, USD) -> 1.2143,
            (USD, EUR) -> 0.823402,

            (EUR, PLN) -> 4.47063,
            (PLN, EUR) -> 0.223682,

            (USD, PLN) -> 3.68113,
            (PLN, USD) -> 0.271655,

            (PLN, PLN) -> 1,
            (USD, USD) -> 1,
            (EUR, EUR) -> 1
        )

        def exchangeRate(from: Currency, to: Currency) = conversion((from, to))
    }

    def main(args: Array[String]): Unit = {
        implicit val converter = CurrencyConverterMock

        println(s"${100.01($)} + ${200(`€`)} = ${100.01($) + 200(`€`)}")
        println(s"${100.01(zł)} + ${200($)} = ${100.01(zł) + 200($)}")
        println(s"${5(zł)} + ${3(PLN)} + ${20.5(USD)} = ${5(zł) + 3(PLN) + 20.5(USD)}")

        println(s"${300.01(USD)} - ${200(EUR)} = ${300.01(USD) - 200(EUR)}")

        println(s"${30(zł)} * 20 = ${30(zł) * 20}")
        println(s"${20($)} * 11 = ${20($) * 11}")

        println(s"${150.01(USD)} as PLN = ${150.01(USD) as PLN}")
        println(s"${120.01(USD)} as € = ${120.01(USD) as `€`}")

        println(s"${300.30(USD)} > ${200(`€`)} = ${300.30(USD) > 200(`€`)}")
        println(s"${300.30(USD)} < ${200(EUR)} = ${300.30(USD) < 200(EUR)}")
    }
}
