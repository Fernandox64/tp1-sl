forall a b . func map(f : (a) -> b, v : a[]) : b[] {
    let result : b[] = new b[v.size];
    let i : int = 0;
    while (i < v.size) {
        result[i] = f(v[i]);
        i = i + 1;
    }
    return result;
}

func inc(x : int) : int {
    return x + 1;
}

func main() : void {
    let v : int[5] = [1, 2, 3, 4, 5];
    let r : int[] = map(inc, v);

    let i : int = 0;
    while (i < 5) {
        print(r[i]);
        i = i + 1;
    }
}
