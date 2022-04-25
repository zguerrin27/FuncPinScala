object exercises {

    // 2.1 - Write Fib
    // 0 and 1 are the first two numbers in the sequence, so we start the accumulators with those. At every
    // iteration, we add the two numbers to get the next one.
    def fib(n: Int): Int = {
        @annotation.tailrec
        def loop(n: Int, prev: Int, cur: Int): Int =
            if (n == 0) prev    
            else loop(n - 1, cur, prev + cur)

        loop(n, 0, 1)
    }

    // 2.2 - Implement method that checks whether an Array[A] is sorted according to a given comparison fucntion
    def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
        @annotation.tailrec
        def loop(n: Int): Int =
            if (n >= as.length - 1) true // if current index is the end of the array, it must be sorted - return true 
            else if (ordered(as(n), as(n+1))) false // if the comparison function returns true, adjacent items are not sorted
            else loop(n+1)

        loop(0)
    }

    // 2.3 - Curry partials function
    //  ( f: (a,b) => c ) can be substituted to c
    // the answer is a => b => (the result of f(a,b)... aka c)
    def curry[A,B,C](f: (A,B) => C): A => (B => C) = 
        a => b => f(a, b)

    // 2.4 - Un curry it 
    def uncurry[A,B,C](f: A => B => C): (A,B) => C =
        (a, c) => f(a)(b)

    // 2.5 - implement a function that composes two other functions
    def compose[A,B,C](f: B => C, g: A => B): A => C =
        a => f(g(a))

    // 3.2 - Implement tail which removes the head of the list
    def tail[A](a: List[A]): List[A] = a match {
        case head :: next => next
        case Nil => sys.error(s"Tail of list is empty")
    }

    // 3.3
    def setHead[A](newHead: A, a: List[A]): List[A] = a match {
        case head :: next => newHead :: next
        case Nil => sys.error("Cant set head on empty list")
    }

    // 3.4 - remove n elements from fron of list 
    def drop[A](n: Int, l: List[A]): List[A] = {

        def loop(list: List[A], num: Int) =  list match {
            case list.length < num => sys.error("list is too short") 
            case head :: next && num == 1 => next
            case head :: next => loop(next, num - 1)
            case Nil => sys.error("empty list") 
        }

        loop(next, n)
    }
    def drop[A](l: List[A], n: Int): List[A] =
        if (n <= 0) l
        else l match {
            case Nil => Nil
            case Cons(_,t) => drop(t, n-1)
    }



    




}