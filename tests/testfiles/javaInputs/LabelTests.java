class LabelTests {
  public int localCopies(int x1, int x2) {
    int t1 = x1;
    int t2 = t1;
    int t3 = t2;
    int t4 = t3;
    return t4;
  }

  public int simpleBranch(int x1, int x2) {
    int z;
    if(x2 > 100)
      z = x2;
    else
      z = x1;

    return z;
  }

  /**
     Should return a phi node
  */
  public int simpleLoop(int x1, int x2) {
    int z = x1;
    while(x2 > 0) {
      z = z + x1;
      --x2;
    }

    return z;
  }

  /**
     Despite the loop body, we are just returning one of the arguments
  */
  public int loopNopBody(int x1, int x2) {
    int z = x1;
    while(x2 > 0) {
      z = z + x1;
      -- x2;
    }

    return x1;
  }

  /**
     Since the exception is not caught, a simple value should be
     returned
  */
  public int uncaughtNPE(String[] s) {
    int x;
    x = s.length;
    return x;
  }

  /**
     Since we have exceptional control flow available,
     this test should return a phi node if everything is working.
  */
  public int simpleNPECatchNPE(String[] s) {
    int x;
    try {
      x = s.length;
    }
    catch(NullPointerException ex) {
      x = 0;
    }

    return x;
  }

  /**
     Runtime exception handlers subsume NPEs, so this should also
  return a phi node
  */
  public int simpleNPECatchRuntimeEx(String[] s) {
    int x;
    try {
      x = s.length;
    }
    catch(RuntimeException ex) {
      x = 0;
    }

    return x;
  }

  /**
     Likewise Exception
  */
  public int simpleNPECatchException(String[] s) {
    int x;
    try {
      x = s.length;
    }
    catch(Exception ex) {
      x = 0;
    }

    return x;
  }

  /**
     And throwable
  */
  public int simpleNPECatchThrowable(String[] s) {
    int x;
    try {
      x = s.length;
    }
    catch(Throwable ex) {
      x = 0;
    }

    return x;
  }

  /**
     In this case, there should be no edge to the handler
     because arraylength cannot produce ArithmeticExceptions.
  */
  public int simpleNPECatchArithEx(String[] s) {
    int x;
    try {
      x = s.length;
    }
    catch(ArithmeticException ex) {
      x = 0;
    }

    return x;
  }
}
