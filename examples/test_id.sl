forall a . func id(x : a) : a {
    return x;
}

func main() : int {
    let r : int = id(42);
    return r;
}