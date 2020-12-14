package pizzeria {

    trait PizzaType {
        def price: BigDecimal
    }

    case object Margarita extends PizzaType {
        override def toString = "Margarita"
        def price = 5
    }

    case object Pepperoni extends PizzaType {
        override def toString = "Pepperoni"
        def price = 6.5
    }

    case object Funghi extends PizzaType {
        override def toString = "Funghi"
        def price = 7
    }


    trait PizzaSize {
        def priceMultiplier: BigDecimal
    }

    case object Small extends PizzaSize {
        override def toString = "Small"
        def priceMultiplier = 0.9
    }
    case object Regular extends PizzaSize {
        override def toString = "Regular"
        def priceMultiplier = 1.0
    }
    case object Large extends PizzaSize {
        override def toString = "Large"
        def priceMultiplier = 1.1
    }


    trait PizzaCrust
    case object Thin  extends PizzaCrust { override def toString = "Thin"}
    case object Thick extends PizzaCrust { override def toString = "Thick"}


    trait PizzaTopping {
        def price: BigDecimal
    }

    case object Ketchup extends PizzaTopping {
        override def toString = "Ketchup"
        def price = 0.5
    }

    case object Garlic extends PizzaTopping {
        override def toString = "Garlic"
        def price = 0.5
    }


    trait PizzaMeat {
        def price: BigDecimal
    }

    case object Salami extends PizzaMeat {
        override def toString = "Salami"
        def price = 1
    }


    trait Drink {
        def price: BigDecimal
    }

    case object Lemonade extends Drink {
        override def toString = "Lemonade"
        def price = 2
    }


    case class Pizza(
        kind: PizzaType,
        size: PizzaSize,
        crust: PizzaCrust,
        extraMeat: Option[PizzaMeat]=None,
        extraTopping: Option[PizzaTopping]=None
    ) {
        override def toString() =
            size.toString + ' ' + kind.toString + '\n' +
            "crust: " + crust.toString + '\n' +
            extraMeat.map((m) => "extra meat: "    + m.toString + '\n').getOrElse("") +
            extraTopping.map((t) => "extra topping: " + t.toString + '\n').getOrElse("")

        val price: BigDecimal = {
            val meatPrice: BigDecimal = extraMeat.map(_.price).getOrElse(0)
            val toppingPrice: BigDecimal = extraTopping.map(_.price).getOrElse(0)
            val totalPrice = size.priceMultiplier * (kind.price + meatPrice + toppingPrice)
            totalPrice.setScale(2, BigDecimal.RoundingMode.UP)
        }

        val priceString: String = "$" + price
    }

    package orders {

        case class PhoneNumber(val number: String) {
            private val phoneRegex = "^([+]?[\\s0-9]+)?(\\d{3}|[(]?[0-9]+[)])?([-]?[\\s]?[0-9])+$"
            require(number.matches(phoneRegex), "Phone number must be valid.")

            override def toString() = number
        }

        trait Discount {
            def pizzaDiscount: BigDecimal
            def drinkDiscount: BigDecimal
        }

        case object StudentDiscount extends Discount {
            override def pizzaDiscount = 0.05
            override def drinkDiscount = 0

            override def toString = "student discount"
        }

        case object SeniorDiscount extends Discount {
            override def pizzaDiscount = 0.07
            override def drinkDiscount = 0.07

            override def toString = "senior discount"
        }

        class Order(
            name: String,
            address: String,
            phone: PhoneNumber,
            pizzas: List[Pizza],
            drinks: List[Drink],
            discount: Option[Discount]=None,
            specialInfo: Option[String]=None
        ) {
            override def toString() =
                "Name: " + name + '\n' +
                "Address: " + address + '\n' +
                "Pizzas: \n" + (for (pizza <- pizzas) yield pizza.toString + "\n").mkString +
                "Drinks: "   + (for (drink <- drinks) yield drink.toString + ", ").mkString +
                '\n' +
                discount.map("with " + _.toString + "\n").getOrElse("") +
                specialInfo.map("Additional info: " + _ + '\n').getOrElse("")

            def extraMeatPrice: BigDecimal =
                pizzas.map(_.extraMeat.map(_.price).getOrElse(BigDecimal(0))).sum

            def pizzasPrice: BigDecimal = pizzas.map(_.price).sum

            def drinksPrice: BigDecimal = drinks.map(_.price).sum

            def priceByType(kind: PizzaType): BigDecimal = {
                val pizzasOfType = pizzas filter (_.kind == kind)
                pizzasOfType.map(_.price).sum
            }

            def price: BigDecimal = {
                val pizzaMultiplier = 1.0 - discount.map(_.pizzaDiscount).getOrElse(BigDecimal(0))
                val drinkMultiplier = 1.0 - discount.map(_.drinkDiscount).getOrElse(BigDecimal(0))
                val totalPrice = (pizzaMultiplier * pizzasPrice + drinkMultiplier * drinksPrice)

                totalPrice.setScale(2, BigDecimal.RoundingMode.UP)
            }
        }
    }
}


object Example {

    import pizzeria._
    import orders._

    def printPizza(pizza: Pizza): Unit = {
        print(pizza.toString)
        println(pizza.priceString)
        println()
    }

    def main(args: Array[String]): Unit = {
        val pizza1 = Pizza(Margarita, Large, Thin, extraTopping=Some(Garlic))
        printPizza(pizza1)

        val pizza2 = Pizza(Pepperoni, Small, Thick, extraMeat=Some(Salami))
        printPizza(pizza2)

        val pizza3 = Pizza(Funghi, Regular, Thin)
        printPizza(pizza3)

        val pizza4 = Pizza(Margarita, Large, Thick, Some(Salami), Some(Ketchup))
        printPizza(pizza4)

        val order1 = new Order(
            "Jan Nowak",
            "ul. Jana Pawła II 21/37, 59-300, Lublin",
            PhoneNumber("+48 000 000 000"),
            List(pizza1, pizza2, pizza3, pizza4),
            List(Lemonade, Lemonade),
            Some(StudentDiscount)
        )

        println(order1.toString)

        println(s"Extra meat price: $$${order1.extraMeatPrice}")
        println(s"Pizzas price: $$${order1.pizzasPrice}")
        println(s"Drinks price: $$${order1.drinksPrice}")
        println(s"Price of margaritas: $$${order1.priceByType(Margarita)}")
        println(s"Total order price: $$${order1.price}")
    }
}