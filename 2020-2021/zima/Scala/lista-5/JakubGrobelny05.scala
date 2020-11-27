package plugins {
    abstract class Pluginable {
        def plugin(s: String) = s
    }

    trait Reverting extends Pluginable {
        abstract override def plugin(s: String) = super.plugin(s.reverse)
    }

    trait LowerCasing extends Pluginable {
        abstract override def plugin(s: String) = super.plugin(s.toLowerCase)
    }

    trait SingleSpacing extends Pluginable {
        abstract override def plugin(s: String) = super.plugin(s.replaceAll(" +", " "))
    }

    trait NoSpacing extends Pluginable {
        abstract override def plugin(s: String) = super.plugin(s filter (i => i != ' '))
    }

    trait DuplicateRemoval extends Pluginable {
        abstract override def plugin(s: String): String = {
            val frequencies = (s.toSeq groupBy identity).view.mapValues(_.size)
            super.plugin(s filter (c => frequencies.get(c).getOrElse(0) == 1))
        }
    }

    trait Rotating extends Pluginable {
        abstract override def plugin(s: String) = super.plugin(s.takeRight(1) + s.take(s.size - 1))
    }

    trait Doubling extends Pluginable {
        abstract override def plugin(s: String): String = super.plugin((s map (c => f"$c$c")).mkString)
    }

    trait Shortening extends Pluginable {
        abstract override def plugin(s: String): String = {
            val shortened = s.zipWithIndex.collect {case (c,i) if (i % 2 == 0) => c}.mkString
            super.plugin(shortened)
        }
    }

    object Actions {
        val actionA = new Pluginable with Shortening with Doubling with SingleSpacing

        val actionB = new Pluginable with Doubling with Shortening with NoSpacing

        val actionC = new Pluginable with Doubling with LowerCasing

        val actionD = new Pluginable with Rotating with DuplicateRemoval

        val actionE = new Pluginable with Reverting with Doubling with Shortening with NoSpacing

        val actionF = new Pluginable with Rotating {
            override def plugin(s: String) = Iterator.iterate(s)(super.plugin).drop(5).next()
        }

        val actionG = new Pluginable {
            override def plugin(s: String) = actionB.plugin(actionA.plugin(s))
        }
    }
}

object Example {

    import plugins._

    def main(args: Array[String]): Unit = {
        val input = "Very Interesting Text"

        val reverter = new Pluginable with Reverting
        println("Reverting: " + reverter.plugin(input))

        val lowercaser = new Pluginable with LowerCasing
        println("LowerCasing: " + lowercaser.plugin(input))

        val singleSpacer = new Pluginable with SingleSpacing
        println("SingleSpacing: " + singleSpacer.plugin(input.replaceAll(" ", "   ")))

        val nospacer = new Pluginable with NoSpacing
        println("NoSpacing: " + nospacer.plugin(input))

        val duplicateRemover = new Pluginable with DuplicateRemoval
        println("DuplicateRemoval: " + duplicateRemover.plugin(input))

        val rotator  = new Pluginable with Rotating
        println("Rotating: " + rotator.plugin(input))

        val doubler = new Pluginable with Doubling
        println("Doubling: " + doubler.plugin(input))

        val shortener = new Pluginable with Shortening
        println("Shortening: " + shortener.plugin(input))

        println("actionA: " + Actions.actionA.plugin(input.replaceAll(" ", "   ")))
        println("actionB: " + Actions.actionB.plugin(input))
        println("actionC: " + Actions.actionC.plugin(input))
        println("actionD: " + Actions.actionD.plugin(input))
        println("actionE: " + Actions.actionE.plugin(input))
        println("actionF: " + Actions.actionF.plugin(input))
        println("actionG: " + Actions.actionG.plugin(input))
    }
}