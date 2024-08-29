package cosmo

// def println(x: Any): Unit = Console.println(x)

def debugln(f: => Any): Unit = {}
def logln(f: => Any): Unit = println(f)
