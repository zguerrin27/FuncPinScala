object exercises {

    // write fib
    def fib(n: Int): Int = n match {
        case 0 => 0
        case 1 => 1
        case _ => fib(n - 1) + fib(n - 2)
    }

}