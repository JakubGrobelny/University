using System;
using System.Collections.Generic;

namespace Zadanie2 {

    public class Context {
        Dictionary<string, bool> context = new Dictionary<string, bool>();

        public bool GetValue(string varName) {
            try {
                return this.context[varName];
            } catch (KeyNotFoundException) {
                throw new ArgumentException(
                    String.Format("Undefined variable \"{0}\"", varName)
                );
            }
        }
 
        public void SetValue(string varName, bool value) {
            this.context[varName] = value;
        }
    }

    public abstract class AbstractExpression {
        public abstract bool Interpret(Context context);
    }

    public class VarExpression : AbstractExpression {
        string name;

        public VarExpression(string name) {
            this.name = name;
        }

        public override bool Interpret(Context context) {
            return context.GetValue(this.name);
        }
    }

    public class ConstExpression : AbstractExpression {
        bool value;

        public ConstExpression(bool value) {
            this.value = value;
        }

        public override bool Interpret(Context context) => this.value;
    }

    public abstract class BinaryExpression : AbstractExpression {
        protected AbstractExpression lhs;
        protected AbstractExpression rhs;

        protected BinaryExpression(
            AbstractExpression lhs, 
            AbstractExpression rhs
        ) {
            this.lhs = lhs;
            this.rhs = rhs;
        }
    }
    
    public abstract class UnaryExpression : AbstractExpression {
        protected AbstractExpression operand;

        protected UnaryExpression(AbstractExpression operand) {
            this.operand = operand;
        }
    }
    
    public class AndExpression : BinaryExpression {
        public AndExpression(AbstractExpression lhs, AbstractExpression rhs)
            : base(lhs, rhs) {}

        public override bool Interpret(Context context) {
            return this.lhs.Interpret(context) && this.rhs.Interpret(context);
        }
    }

    public class OrExpression : BinaryExpression {
        public OrExpression(AbstractExpression lhs, AbstractExpression rhs)
            : base(lhs, rhs) {}

        public override bool Interpret(Context context) {
            return this.lhs.Interpret(context) || this.rhs.Interpret(context);
        }
    }

    public class NotExpression : UnaryExpression {
        public NotExpression(AbstractExpression operand)
            : base(operand) {}

        public override bool Interpret(Context context) {
            return !this.operand.Interpret(context);
        }
    }

    public class Example {
        public static void Main() {
            var context = new Context();
            context.SetValue("p", true);
            context.SetValue("q", false);
            context.SetValue("r", true);

            // p ∧ r ∨ ¬(⊥ ∨ q)
            AbstractExpression expr1 = new OrExpression(
                new AndExpression(
                    new VarExpression("p"),
                    new VarExpression("r")
                ),
                new NotExpression(
                    new OrExpression(
                        new ConstExpression(false),
                        new VarExpression("q")
                    )
                )
            );

            Console.WriteLine(
                "Value of the expression 1: {0}",
                expr1.Interpret(context)
            );

            // Niezdefiniowana zmienna.
            AbstractExpression expr2 = new NotExpression(
                new VarExpression("undefined")
            );

            try { 
                expr2.Interpret(context);
            } catch (Exception exc) {
                Console.Error.WriteLine("Caught exception:\n {0}", exc);
            }
        }
    }
}