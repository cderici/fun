import threading, time, random, Queue

TIME_TO_RUN = 10

class Producer:
    def __init__(self):
        self.product = ['a','b','c','d','e','f','g']
        self.next = 0

    def run(self):
        global q
        while time.clock() < TIME_TO_RUN:
            if self.next < time.clock():
                f = self.product[random.randrange(len(self.product))]
                q.put(f)
                print "Added -- %s\n" % f
                self.next += random.random()

class Consumer:
    def __init__(self):
        self.next = 0

    def run(self):
        global q
        while time.clock() < TIME_TO_RUN:
            if self.next < time.clock():
                if not q.empty():
                    f = q.get()
                    print "Removed ---- %s\n" % f
                    self.next += random.random()
                else:
                    print "Consumer is waiting \n"
if __name__ == '__main__':
    q = Queue.Queue(10)
    p = Producer()
    c = Consumer()

    pt = threading.Thread(target=p.run, args=())
    pt.daemon = True
    ct = threading.Thread(target=c.run, args=())

    pt.start()
    ct.start()
