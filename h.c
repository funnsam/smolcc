// ptr(fn(int a))
//     ~~2
// ~~~1
//
// ptr(int)
// fn(ptr(int))
// int *(a()) {}

// fn(ptr(int b))
//    ~~~2
// ~~1
//
// fn(int)
// ptr(fn(int))
// int (*b)() {}

// fn(ptr(array(int, 4, p)), char)
//        ~~~~~3
//    ~~~2
// ~~                        ~~~~1
//
// fn(int, [char])
// ptr(fn(int, [char]))
// array(ptr(fn(int, [char])), 4)
// int (*(p[4])) (char);

// struct h { int x; } s[10] = { [0].x = 1 }; */

long
long main {
}
void main {
}
