// The SSA transformation should eliminate some kinds of unused
// values.  In particular, it should not create phi nodes that are
// never used.
class OneUnusedPhi {
  static int target;
  static void entry(int x) {
    int i1;
    int i2;
    if(x > 0) {
      i1 = x;
      i2 = 12;
    }
    else {
      i1 = 0;
      i2 = 22;
    }

    target = i1;
  }
}
