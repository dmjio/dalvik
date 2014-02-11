class EliminateUnreachableCatch {
  static int entry(int x) {
    int z = x;
    try {
      z = 1;
    }
    catch(NullPointerException ex) {
      z = 2;
    }

    return z;
  }
}
