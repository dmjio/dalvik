class NestedLoop {
  static int entry(int x, int y) {
    int z = 1;
    for(int i = 0; i < x; ++i) {
      for(int j = 0; j < y; ++j) {
        z += j * i;
        i += 1;
      }
    }

    return z;
  }
}
