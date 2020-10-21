function isPrime(num) {
    const sqrt = Math.sqrt(num);
    for (let i = 2; i <= sqrt; i++) {
        if (num % i == 0) {
            return false;
        }
    }
    return true;
}

const primes = Array.from(Array(9999), (v, i) => i + 2).filter(isPrime);
console.log(primes);
