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

  public int simpleLoop(int x1, int x2) {
    int z = x1;
    while(x2 > 0) {
      z = z + x1;
      --x2;
    }

    return z;
  }

  public int loopNopBody(int x1, int x2) {
    int z = x1;
    while(x2 > 0) {
      z = z + x1;
      -- x2;
    }

    return x1;
  }
}
