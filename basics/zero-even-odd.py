from threading import Semaphore


class ZeroEvenOdd(object):

    def __init__(self, n):
        self.n = n
        self.zero_sem = Semaphore(1)
        self.even_sem = Semaphore(0)
        self.odd_sem = Semaphore(0)


    # printNumber(x) outputs "x", where x is an integer
    def zero(self, printNumber):
        for i in range(self.n):
            self.zero_sem.acquire()
            printNumber(0)
            (self.even_sem if i % 2 else self.odd_sem).release()

    def even(self, printNumber):
        for i in range(2, self.n+1, 2):
            self.even_sem.acquire()
            printNumber(i)
            self.zero_sem.release()

    def odd(self, printNumber):
        for i in range(1, self.n+1, 2):
            self.odd_sem.acquire()
            printNumber(i)
            self.zero_sem.release()

z1 = ZeroEvenOdd(2)

t1.start() # will call zero method in its run method
t2.start() # will call even method in its run method
t3.start() # will call odd method in its run method
