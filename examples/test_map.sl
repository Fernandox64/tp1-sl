forall a b . func map (f : (a) -> b, v : a[]) : b[] {
    let result : b[] = new b[v.size];
    for (let i : int = 0; i < v.size; i = i + 1) {
      result[i] = f(v[i]);
    }
    return result;
}
