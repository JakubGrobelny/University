using System;

namespace Zadanie4 {

    public class CoffeeMachine {
        private IState state;

        public CoffeeMachine() {
            this.state = new IdleState(this);
        }

        public void SetState(IState newState) {
            this.state = newState;
        }

        public void InsertCoins(int amount) {
            this.state.InsertCoins(amount);
        }

        public void ChooseCoffee(int productId) {
            this.state.ChooseCoffee(productId);
        }

        public void TakeCoffee() {
            this.state.TakeCoffee();
        }
    }

    public interface IState {
        void InsertCoins(int amount);

        void ChooseCoffee(int productId);

        void TakeCoffee();
    }   

    public class IdleState : IState {
        CoffeeMachine machine;
        int moneyInside;

        public static readonly int[] PRICES = {
            30, 20, 40, 35
        };

        public IdleState(CoffeeMachine machine, int moneyInside = 0) {
            this.machine = machine;
            this.moneyInside = moneyInside;
        }
        
        public void InsertCoins(int amount) {
            Console.WriteLine("Inserted {0}.", amount);
            this.moneyInside += amount;
        }

        public void ChooseCoffee(int productId) {
            var price = PRICES[productId] - this.moneyInside;
            Console.WriteLine("Chosen coffee number {0}.", productId);
            machine.SetState(new AwaitingMoneyState(machine, price));
        }

        public void TakeCoffee() {
            throw new Exception("There is no coffee to take!");
        }
    }

    public class AwaitingMoneyState : IState {
        CoffeeMachine machine;
        int paymentLeft;

        public AwaitingMoneyState(CoffeeMachine machine, int price) {
            this.paymentLeft = price;
            this.machine = machine;
        }

        public void InsertCoins(int amount) {
            Console.WriteLine("Inserted {0}.", amount);
            this.paymentLeft -= amount;
            if (this.paymentLeft <= 0) {
                var change = -this.paymentLeft;
                this.machine.SetState(
                    new IssuedProductState(this.machine, change)
                );
            }
        }

        public void ChooseCoffee(int productId) {
            throw new Exception("Coffee has already been chosen!");
        }

        public void TakeCoffee() {
            throw new Exception("There is no coffee to take!");
        }
    }

    public class IssuedProductState : IState {
        CoffeeMachine machine;

        public IssuedProductState(CoffeeMachine machine, int change) {
            this.machine = machine;
            Console.WriteLine("Given change: {0}.", change);
        }

        public void InsertCoins(int amount) {
            throw new Exception("Cannot insert coins at this point!");
        }

        public void ChooseCoffee(int productId) {
            throw new Exception("Take your coffee first before next order!");
        }

        public void TakeCoffee() {
            Console.WriteLine("Coffee taken.");
            this.machine.SetState(new IdleState(machine));
        }
    }

    public class Example {
        public static void Main() {
            var machine = new CoffeeMachine();
            machine.InsertCoins(7);
            machine.ChooseCoffee(1);
            machine.InsertCoins(10);
            machine.InsertCoins(7);
            machine.TakeCoffee();
        }
    }
}