import sys

def game():
    args = []
    inventory = []
    def setargs(l):
        nonlocal args
        args = l.rstrip()
    def getPromptObj(l):
        return ("p",l,setargs)
    def getNextObj():
        return ("n",)

    print("You wake up to find yourself laying down inside a dark cave.")
    print("What will you do?")
    yield getPromptObj((("0","Check your bag"), ("1","Call for help")))

    if args == "0":
        print("You found a lighter inside your bag.")
        inventory.append("lighter")
    elif args == "1":
        print("Your voice echos through the cave...")
    yield getNextObj()

    while True:
        print("What will you do next?")
        yield getPromptObj((("0","Stand up"), ("1", "Continue laying down")))
        if args == "0":
            print("You find a way leading outside of the cave.")
            break
        else:
            print("You spend several minutes in the dark, but no help seems to arrive...")
    print("To be continued...")

def gameloop(game):
    for o in game:
        if not o:
            break
        if o[0] == "n":
            print("Hit enter to continue...")
            sys.stdin.readline()
            yield ""
        elif o[0] == "p":
            d = o[1]
            for k in d:
                print(k[0], k[1])
            print("> ",end='',flush=True)
            choice = sys.stdin.readline()
            o[2](choice)
            yield ""

def trynext(g):
    try:
        next(g)
    except Exception as e:
        pass

if __name__ == '__main__':
    for i in gameloop(game()):
        pass