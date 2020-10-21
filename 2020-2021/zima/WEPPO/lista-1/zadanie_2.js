function divisibleByDigitsAndTheirSum(num) {
    function divisibleByAll(digits) {
        return digits.every((digit) => num % digit == 0);
    }

    function divisibleBySum(digits) {
        return num % digits.reduce((a, b) => a + b, 0) == 0;
    }

    let digits = num.toString().split('').map(Number);
    return divisibleByAll(digits) && divisibleBySum(digits);
}

const numbers = Array.from(Array(10000), (v, i) => i + 1).filter(divisibleByDigitsAndTheirSum);
console.log(numbers);
