use python_macro::python;

fn main() {
    python! {
        print("Hello, world")
    }
    
    python! {
        for x in range(3):
            print(x)
    }
}
