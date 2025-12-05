struct Person {
    name : string;
    age : int;
    height : float;
}

func main() : void {
    let p : Person = Person{"Alice", 25, 170.0};
    let people : Person[3] = [p, p, p];

    let i : int = 0;
    while (i < 3) {
        print(people[i].name);
        print(people[i].age);
        print(people[i].height);
        i = i + 1;
    }
}