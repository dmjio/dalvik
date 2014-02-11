class SimpleLoop {
  static int entry(int x) {
    int z = 0;
    for(int i = 0; i < x; ++i) {
      z += x;
    }

    return z;
  }
}
