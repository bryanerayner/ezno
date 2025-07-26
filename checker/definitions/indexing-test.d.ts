
// interface Array<T> {
//     [key: number]: T;
//     length: number;
// }

interface ArrayKlass<T> {
    [key: number]: T;
    length: number;
}

declare class ArrayK<T> {
    [index: number]: T;
}
